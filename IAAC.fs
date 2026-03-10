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
     erasedHuffman.Reset()
     controlHuffman.Reset()
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
             erasedHuffman.Update(fmin)
             Array.Clear leadFreqArray
             Array.Clear trailFreqArray
         //进行预处理
         let struct (doErase, betaStar, v) = eraseTrailZero data[i - 1]
         let eraseControl = if doErase then betaStar * 2 + 1 else 0
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
         //先获取擦除相关的控制位
         let struct (erasedControlHuff, erasedHuffBitCount) =
             erasedHuffman.Encode eraseControl

         writer.WriteBits(uint64 erasedControlHuff, erasedHuffBitCount)
         //开始写入
         match entry.Mode with
         | 0 -> //和上一个数相同
             let struct (controlHuff, huffBitCount) = controlHuffman.Encode(0)
             writer.WriteBits(uint64 controlHuff, huffBitCount)
         | 2 -> //直接复用
             let struct (controlHuff, huffBitCount) = controlHuffman.Encode(2)
             writer.WriteBits(uint64 controlHuff, huffBitCount)
             writer.WriteBits(xor >>> lastTz, 64 - lastLz - lastTz)
         | 5 -> //写入前导0和尾随0
             let mutable control = 5
             //写入前导0和尾随0
             control <- control ||| (controlLz <<< 3) ||| (controlTz <<< (3 + leadBitCount))
             let struct (controlHuff, huffBitCount) = controlHuffman.Encode(control)
             writer.WriteBits(uint64 controlHuff, huffBitCount)
             writer.WriteBits(xor >>> curTz, 64 - curLz - curTz)
         | 7 -> //复用前导0 记录尾随0
             let mutable control = 7
             control <- control ||| (controlTz <<< (3))
             let struct (controlHuff, huffBitCount) = controlHuffman.Encode(control)
             writer.WriteBits(uint64 controlHuff, huffBitCount)
             writer.WriteBits(xor >>> curTz, 64 - lastLz - curTz)
         | 3 -> //尾随0太少直接写入除尾随0外的所有数
             let mutable control = 3
             let struct (controlHuff, huffBitCount) = controlHuffman.Encode(control)
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
    erasedHuffman.Reset()
    controlHuffman.Reset()
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
            erasedHuffman.Update(fmin)
            Array.Clear leadFreqArray
            Array.Clear trailFreqArray

        // ── 步骤 1：解码擦除控制位 ──
        let struct(erasedControl,erasedControlBit) = erasedHuffman.Decode(reader)

        // ── 步骤 2：解码算法选择控制位 ──
        let struct(control,controlBit) = controlHuffman.Decode(reader)

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
