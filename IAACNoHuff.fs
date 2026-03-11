module Compress.IAACNoHuff

open System
open Compress.ElfUtil
open Compress.IAACutil
open Compress.toolClass

type IAACNoHuff() =
    interface ICompressor with
        member _.GetName() =
            "IAACNoHuff"
        member _.Compress (writer, ptrBuffer) =
            let data = ptrBuffer.AsSpan()
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
        
        member _.Decompress (reader:BitReader,count: int) : double[] =
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
        
        