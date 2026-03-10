module Compress.IAAC

open System
open Compress.ElfUtil
open Compress.LUT
open Compress.toolClass
open Compress.IAACutil


let compress (writer: BitWriter) (data: double[]) =
     if data.Length = 0 then
         invalidArg "data" "empty input"
     // 初始化
     huffman.Reset()
     Array.Clear leadFreqArray
     Array.Clear trailFreqArray   
     let leadFreqSpan = ReadOnlySpan(leadFreqArray, 0, 64)
     let trailFreqSpan = ReadOnlySpan(trailFreqArray, 0, 64)
     let leadModule = zeroLeadModule.Copy()
     let trailModule = zeroTrailModule.Copy()
     //开始写入
     let mutable last = 0UL
     let mutable lastLz = 64
     let mutable lastTz = 64

     for i = 1 to data.Length do
         if i % evalThreshold = 0 then
             leadModule.Update leadFreqSpan
             trailModule.Update trailFreqSpan
             huffman.Update(fmin)
             Array.Clear leadFreqArray
             Array.Clear trailFreqArray
         //进行预处理
         let struct (doErase, betaStar, v) = eraseTrailZero data[i - 1]
         let eraseControl = if doErase then betaStar * 2 + 1 else 0
         let eraseControlBit = if doErase then 5 else 1
         let xor = v ^^^ last
         let lz = BitUtil.lz xor
         let tz = BitUtil.tz xor
         let deltaLz = lz - lastLz
         let deltaTz = tz - lastTz
         leadFreqArray[lz] <- leadFreqArray[lz] + 1
         trailFreqArray[tz] <- trailFreqArray[tz] + 1
         //先确定编码模式
         let controlLz = (leadModule.GetControl(lz))
         let curLz = (leadModule.GetNumFromControl(int controlLz))
         let controlTz = (trailModule.GetControl(tz))
         let curTz = trailModule.GetNumFromControl(int controlTz)
         //二层查表获取编码方式
         let entry = lut.Lookup(deltaLz, deltaTz, curTz)
         //开始写入
         match entry.Mode with
         | 0 -> //和上一个数相同
             let struct (controlHuff, huffBitCount) = huffman.Encode(eraseControl)
             writer.WriteBits(uint64 controlHuff, huffBitCount)
         | 2 -> //直接复用
             let control = eraseControl ||| (2 <<< eraseControlBit)
             let struct (controlHuff, huffBitCount) = huffman.Encode(control)
             writer.WriteBits(uint64 controlHuff, huffBitCount)
             writer.WriteBits(xor >>> lastTz, 64 - lastLz - lastTz)
         | 5 -> //写入前导0和尾随0
             let mutable control = 5
             //写入前导0和尾随0
             control <- control ||| (controlLz <<< 3) ||| (controlTz <<< (3 + leadBitCount))
             //把擦除控制位拼接到前面
             control <- eraseControl ||| (control <<< eraseControlBit)
             let struct (controlHuff, huffBitCount) = huffman.Encode(control)
             writer.WriteBits(uint64 controlHuff, huffBitCount)
             writer.WriteBits(xor >>> curTz, 64 - curLz - curTz)
         | 7 -> //复用前导0 记录尾随0
             let mutable control = 7
             control <- control ||| (controlTz <<< (3))
             //把擦除控制位拼接到前面
             control <- eraseControl ||| (control <<< eraseControlBit)
             let struct (controlHuff, huffBitCount) = huffman.Encode(control)
             writer.WriteBits(uint64 controlHuff, huffBitCount)
             writer.WriteBits(xor >>> curTz, 64 - lastLz - curTz)
         | 3 -> //尾随0太少直接写入除尾随0外的所有数
             let mutable control = 3
             control <- eraseControl ||| (control <<< (eraseControlBit))
             let struct (controlHuff, huffBitCount) = huffman.Encode(control)
             writer.WriteBits(uint64 controlHuff, huffBitCount)
             writer.WriteBits(xor , 64 - lastLz)
         | _ -> raise (Exception("未知的模式位"))

         last <- v
         lastLz <- curLz
         lastTz <- curTz

     writer.ToArray()


/// 解压缩
// ════════════════════════════════════════════════════════
// IAAC 解码端
// 与编码端严格对称，维护完全相同的状态以保证校准同步
// ════════════════════════════════════════════════════════

/// 解码端（与编码端严格对称）
///
/// 编码端模式 → Huffman 符号低 3 位对应关系：
///   entry.Mode 0 → control = 0   (000)  值相同，xor=0
///   entry.Mode 1 → control = 2   (010)  复用 lastLz/lastTz
///   entry.Mode 5 → control & 7 = 5 (101)  新 lead + trail
///   entry.Mode 7 → control & 7 = 7 (111)  复用 lead，新 trail
///   entry.Mode 6 → control = 3   (011)  尾随零太少，不编码 trail

let decompress (data: uint64[]) (count: int) : double[] =
    if count = 0 then
        raise (ArgumentException("待解压数据不可为空"))

    let reader = BitReader data
    let result = Array.zeroCreate<double> count

    // ── 初始化（与编码端完全一致）──
    huffman.Reset()
    Array.Clear leadFreqArray
    Array.Clear trailFreqArray
    let leadFreqSpan = ReadOnlySpan(leadFreqArray, 0, 64)
    let trailFreqSpan = ReadOnlySpan(trailFreqArray, 0, 64)
    let leadModule = zeroLeadModule.Copy()
    let trailModule = zeroTrailModule.Copy()

    let mutable last = 0UL
    let mutable lastLz = 64
    let mutable lastTz = 64

    for i = 1 to count do
        // ── 周期性主动校准（与编码端触发时机完全一致）──
        if i % evalThreshold = 0 then
            leadModule.Update leadFreqSpan
            trailModule.Update trailFreqSpan
            huffman.Update(fmin)
            Array.Clear leadFreqArray
            Array.Clear trailFreqArray

        //获取整个控制位
        let struct(allControl,allControlBit) = huffman.Decode(reader)
        // ── 步骤 1：解码擦除控制位 ──
        let struct(erasedControl,erasedControlBit) =
            //不进行擦除
            if (allControl &&& 1) = 0 then
                struct(0,1)
            else
                struct(allControl &&& 0x1F,5)

        // ── 步骤 2：解码算法选择控制位 ──
        let struct(control,controlBit) = struct(allControl >>> erasedControlBit,allControlBit - erasedControlBit)

        // ── 步骤 3：根据控制位低 3 位确定模式，读取有效位，重建 xor ──
        let mode = control &&& 7
        let mutable xor = 0UL
        let mutable curLz = lastLz
        let mutable curTz = lastTz

        match mode with
        | 0 ->
            // ── 值相同，xor = 0 ──
            // 编码端: huffman.Encode(0)，无 payload
            xor <- 0UL

        | 2 ->
            // ── 复用 lastLz 和 lastTz ──
            // 编码端: WriteBits(xor >>> lastTz, 64 - lastLz - lastTz)
            let centerBits = 64 - lastLz - lastTz
            if centerBits > 0 then
                let center = reader.ReadBits(centerBits)
                xor <- center <<< lastTz

        | 5 ->
            // ── 新 lead + trail ──
            // 编码端: control = 5 ||| (controlLz <<< 3) ||| (controlTz <<< (3 + leadBitCount))
            //         WriteBits(xor >>> curTz, 64 - curLz - curTz)
            let controlLz = (control >>> 3) &&& ((1 <<< leadBitCount) - 1)
            let controlTz = (control >>> (3 + leadBitCount)) &&& ((1 <<< trailBitCount) - 1)
            curLz <- leadModule.GetNumFromControl(controlLz)
            curTz <- trailModule.GetNumFromControl(controlTz)
            let centerBits = 64 - curLz - curTz
            if centerBits > 0 then
                let center = reader.ReadBits(centerBits)
                xor <- center <<< curTz

        | 7 ->
            // ── 复用 lead，新 trail ──
            // 编码端: control = 7 ||| (controlTz <<< 3)
            //         WriteBits(xor >>> curTz, 64 - lastLz - curTz)
            let controlTz = (control >>> 3) &&& ((1 <<< trailBitCount) - 1)
            curTz <- trailModule.GetNumFromControl(controlTz)
            let centerBits = 64 - lastLz - curTz
            if centerBits > 0 then
                let center = reader.ReadBits(centerBits)
                xor <- center <<< curTz

        | 3 ->
            // ── 尾随零太少，不编码 trail ──
            // 编码端: control = 3
            //         WriteBits(xor >>> curTz, 64 - lastLz)
            // LUT 仅在 curTz ≈ 0 时选此模式，payload 即为 xor 的低 (64-lastLz) 位
            let payloadBits = 64 - lastLz
            if payloadBits > 0 then
                let payload = reader.ReadBits(payloadBits)
                xor <- payload

        | _ -> raise (Exception($"未知的模式位: {mode}"))

        // ── 步骤 4：恢复擦除后的浮点数 ──
        let v = xor ^^^ last

        // ── 步骤 5：执行擦除逆操作，还原原始浮点数 ──
        let originalDouble =
            if erasedControl = 0 then
                BitConverter.Int64BitsToDouble(int64 v)
            else
                let betaStar = erasedControl / 2
                recoveryTrailZero (BitUtil.u2d v) betaStar

        result.[i - 1] <- originalDouble

        // ── 步骤 6：更新频率统计（使用实际值，与编码端一致）──
        let lz = BitUtil.lz xor
        let tz = BitUtil.tz xor
        leadFreqArray.[lz] <- leadFreqArray.[lz] + 1
        trailFreqArray.[tz] <- trailFreqArray.[tz] + 1

        // ── 步骤 7：统一从实际值计算 curLz/curTz ──
        // 编码端每轮: curLz = leadModule.GetNumFromControl(leadModule.GetControl(lz))
        // 无论哪个模式，lastLz/lastTz 始终来自实际 lz/tz 经 ZeroCountModel 近似
        curLz <- leadModule.GetNumFromControl(leadModule.GetControl(lz))
        curTz <- trailModule.GetNumFromControl(trailModule.GetControl(tz))

        // ── 步骤 8：更新状态（与编码端严格对齐）──
        last <- v
        lastLz <- curLz
        lastTz <- curTz

    result
// ════════════════════════════════════════════════════════
// IAAC 无哈夫曼版本
// 使用固定前缀码替代自适应哈夫曼
//
// 擦除控制：1 bit 标志 + 条件 4 bit betaStar
//
// 模式前缀码（LSB-first）：
//   Mode 0 → 0b00   (2 bit)  值相同，xor=0
//   Mode 2 → 0b01   (2 bit)  复用 lastLz/lastTz
//   Mode 5 → 0b10   (2 bit)  新 lead + trail
//   Mode 7 → 0b011  (3 bit)  复用 lead，新 trail
//   Mode 3 → 0b111  (3 bit)  尾随零太少，不编码 trail
// ════════════════════════════════════════════════════════

let compressNoHuff (writer: BitWriter) (data: double[]) =
    if data.Length = 0 then
        invalidArg "data" "empty input"

    // 初始化
    Array.Clear leadFreqArray
    Array.Clear trailFreqArray
    let leadFreqSpan = ReadOnlySpan(leadFreqArray, 0, 64)
    let trailFreqSpan = ReadOnlySpan(trailFreqArray, 0, 64)
    let leadModule = zeroLeadModule.Copy()
    let trailModule = zeroTrailModule.Copy()

    let mutable last = 0UL
    let mutable lastLz = 64
    let mutable lastTz = 64

    for i = 1 to data.Length do
        // 周期性校准
        if i % evalThreshold = 0 then
            leadModule.Update leadFreqSpan
            trailModule.Update trailFreqSpan
            Array.Clear leadFreqArray
            Array.Clear trailFreqArray

        // 预处理
        let struct (doErase, betaStar, v) = eraseTrailZero data[i - 1]
        let xor = v ^^^ last
        let lz = BitUtil.lz xor
        let tz = BitUtil.tz xor
        leadFreqArray[lz] <- leadFreqArray[lz] + 1
        trailFreqArray[tz] <- trailFreqArray[tz] + 1

        // 确定编码模式
        let controlLz = leadModule.GetControl(lz)
        let curLz = leadModule.GetNumFromControl(int controlLz)
        let controlTz = trailModule.GetControl(tz)
        let curTz = trailModule.GetNumFromControl(int controlTz)

        // 查表获取编码方式
        let entry = lut.Lookup(lz - lastLz, tz - lastTz, curTz)

        // ── 写擦除控制位 ──
        if doErase then
            writer.WriteBits(1UL, 1)
            writer.WriteBits(uint64 betaStar, 4)
        else
            writer.WriteBits(0UL, 1)

        // ── 写模式前缀 + payload ──
        match entry.Mode with
        | 0 ->
            // 值相同，xor = 0
            writer.WriteBits(0b00UL, 2)

        | 2 ->
            // 复用 lastLz 和 lastTz
            writer.WriteBits(0b01UL, 2)
            let centerBits = 64 - lastLz - lastTz
            if centerBits > 0 then
                writer.WriteBits(xor >>> lastTz, centerBits)

        | 5 ->
            // 新 lead + trail
            writer.WriteBits(0b10UL, 2)
            writer.WriteBits(uint64 controlLz, leadBitCount)
            writer.WriteBits(uint64 controlTz, trailBitCount)
            let centerBits = 64 - curLz - curTz
            if centerBits > 0 then
                writer.WriteBits(xor >>> curTz, centerBits)

        | 7 ->
            // 复用 lead，新 trail（前缀 011，LSB-first）
            writer.WriteBits(0b011UL, 3)
            writer.WriteBits(uint64 controlTz, trailBitCount)
            let centerBits = 64 - lastLz - curTz
            if centerBits > 0 then
                writer.WriteBits(xor >>> curTz, centerBits)

        | 3 ->
            // 尾随零太少，不编码 trail（前缀 111，LSB-first）
            writer.WriteBits(0b111UL, 3)
            let payloadBits = 64 - lastLz
            if payloadBits > 0 then
                writer.WriteBits(xor, payloadBits)

        | _ -> raise (Exception("未知的模式位"))

        // 更新状态
        last <- v
        lastLz <- curLz
        lastTz <- curTz

    writer.ToArray()


// ════════════════════════════════════════════════════════
// 解码端（无哈夫曼，与编码端严格对称）
// ════════════════════════════════════════════════════════

let decompressNoHuff (data: uint64[]) (count: int) : double[] =
    if count = 0 then
        raise (ArgumentException("待解压数据不可为空"))

    let reader = BitReader data
    let result = Array.zeroCreate<double> count

    // 初始化（与编码端完全一致）
    Array.Clear leadFreqArray
    Array.Clear trailFreqArray
    let leadFreqSpan = ReadOnlySpan(leadFreqArray, 0, 64)
    let trailFreqSpan = ReadOnlySpan(trailFreqArray, 0, 64)
    let leadModule = zeroLeadModule.Copy()
    let trailModule = zeroTrailModule.Copy()

    let mutable last = 0UL
    let mutable lastLz = 64
    let mutable lastTz = 64

    for i = 1 to count do
        // 周期性校准（与编码端触发时机完全一致）
        if i % evalThreshold = 0 then
            leadModule.Update leadFreqSpan
            trailModule.Update trailFreqSpan
            Array.Clear leadFreqArray
            Array.Clear trailFreqArray

        // ── 步骤 1：读擦除控制位 ──
        let eraseFlag = reader.ReadBits(1)
        let struct (doErase, betaStar) =
            if eraseFlag = 0UL then
                struct (false, 0)
            else
                let bs = int (reader.ReadBits(4))
                struct (true, bs)

        // ── 步骤 2：读模式前缀（LSB-first，先读低 2 位）──
        let mutable xor = 0UL
        let mutable curLz = lastLz
        let mutable curTz = lastTz

        let prefix = int (reader.ReadBits(2))

        match prefix with
        | 0 ->
            // 0b00 → Mode 0：值相同，xor = 0
            xor <- 0UL

        | 1 ->
            // 0b01 → Mode 2：复用 lastLz 和 lastTz
            let centerBits = 64 - lastLz - lastTz
            if centerBits > 0 then
                let center = reader.ReadBits(centerBits)
                xor <- center <<< lastTz

        | 2 ->
            // 0b10 → Mode 5：新 lead + trail
            let controlLz = int (reader.ReadBits(leadBitCount))
            let controlTz = int (reader.ReadBits(trailBitCount))
            curLz <- leadModule.GetNumFromControl(controlLz)
            curTz <- trailModule.GetNumFromControl(controlTz)
            let centerBits = 64 - curLz - curTz
            if centerBits > 0 then
                let center = reader.ReadBits(centerBits)
                xor <- center <<< curTz

        | 3 ->
            // 0b11 → 再读 1 bit 区分 Mode 7 和 Mode 3
            let extra = int (reader.ReadBits(1))
            if extra = 0 then
                // 0b011 → Mode 7：复用 lead，新 trail
                let controlTz = int (reader.ReadBits(trailBitCount))
                curTz <- trailModule.GetNumFromControl(controlTz)
                let centerBits = 64 - lastLz - curTz
                if centerBits > 0 then
                    let center = reader.ReadBits(centerBits)
                    xor <- center <<< curTz
            else
                // 0b111 → Mode 3：尾随零太少，不编码 trail
                let payloadBits = 64 - lastLz
                if payloadBits > 0 then
                    let payload = reader.ReadBits(payloadBits)
                    xor <- payload

        | _ -> raise (Exception("不可达"))

        // ── 步骤 3：恢复擦除后的浮点数 ──
        let v = xor ^^^ last

        // ── 步骤 4：执行擦除逆操作，还原原始浮点数 ──
        let originalDouble =
            if not doErase then
                BitConverter.Int64BitsToDouble(int64 v)
            else
                recoveryTrailZero (BitUtil.u2d v) betaStar

        result.[i - 1] <- originalDouble

        // ── 步骤 5：更新频率统计（使用实际值，与编码端一致）──
        let lz = BitUtil.lz xor
        let tz = BitUtil.tz xor
        leadFreqArray.[lz] <- leadFreqArray.[lz] + 1
        trailFreqArray.[tz] <- trailFreqArray.[tz] + 1

        // ── 步骤 6：统一从实际值计算 curLz/curTz ──
        curLz <- leadModule.GetNumFromControl(leadModule.GetControl(lz))
        curTz <- trailModule.GetNumFromControl(trailModule.GetControl(tz))

        // ── 步骤 7：更新状态（与编码端严格对齐）──
        last <- v
        lastLz <- curLz
        lastTz <- curTz

    result

