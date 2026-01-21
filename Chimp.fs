module Compress.Chimp

open System
open Compress.toolClass
open toolClass

// =======================================================
// Bit Utilities for CHIMP Algorithm
// =======================================================

module BitUtil =
    let inline lz (x: uint64) =
        System.Numerics.BitOperations.LeadingZeroCount x

    let inline tz (x: uint64) =
        System.Numerics.BitOperations.TrailingZeroCount x

    let d2u (v: double) = BitConverter.DoubleToUInt64Bits v
    let u2d (v: uint64) = BitConverter.UInt64BitsToDouble v

    // CHIMP专用前导零映射函数
    let mapLeadingZeros (lead: int) : uint64 =
        match lead with
        | l when l >= 24 -> 7UL // 24+ → 111
        | l when l >= 22 -> 6UL // 22-23 → 110
        | l when l >= 20 -> 5UL // 20-21 → 101
        | l when l >= 18 -> 4UL // 18-19 → 100
        | l when l >= 16 -> 3UL // 16-17 → 011
        | l when l >= 12 -> 2UL // 12-15 → 010
        | l when l >= 8 -> 1UL // 8-11 → 001
        | _ -> 0UL // 0-7 → 000

    let unmapLeadingZeros (mapped: int) : int =
        match mapped with
        | 7 -> 24
        | 6 -> 22
        | 5 -> 20
        | 4 -> 18
        | 3 -> 16
        | 2 -> 12
        | 1 -> 8
        | 0 -> 0
        | _ -> 0

// =======================================================
// CHIMP Compression Algorithm
// =======================================================

let compress (w:BitWriter)(values: double[]) : uint64[] =
    if values = null || values.Length = 0 then
        raise (ArgumentException("values"))

    let mutable prevD = 0.0
    let mutable prev = 0UL
    let mutable prevMappedLead = 0UL

    for i = 0 to values.Length - 1 do
        let cur = BitUtil.d2u values[i]

        if i = 0 then
            // 第一个值直接存储64位
            w.WriteBits(cur, 64)
            let lead = BitUtil.lz cur
            prevMappedLead <- BitUtil.mapLeadingZeros (lead)
        else
            let xorv = cur ^^^ prev
            let lead = BitUtil.lz xorv
            let trail = BitUtil.tz xorv
            let mappedLead = BitUtil.mapLeadingZeros lead

            if trail > 6 then
                w.WriteBit(0) // 第一个标志位

                if xorv = 0UL then
                    // 情况1: 与前一个值相同
                    w.WriteBit(0)
                else
                    // 情况2: 尾随零较多
                    w.WriteBit(1) // 第二个标志位
                    w.WriteBits(mappedLead, 3)
                    prevMappedLead <- mappedLead
                    let centerBits = 64 - (BitUtil.unmapLeadingZeros (int mappedLead)) - trail
                    w.WriteBits(uint64 centerBits, 6)
                    let center = (xorv >>> trail)
                    w.WriteBits(center, centerBits)
            else
                // 情况3: 尾随零较少
                w.WriteBit(1) // 第一个标志位

                if mappedLead = prevMappedLead then
                    w.WriteBit(0) // 第二个标志位
                    let significantBits = 64 - (BitUtil.unmapLeadingZeros (int mappedLead))
                    w.WriteBits(xorv, significantBits)
                else
                    w.WriteBit(1) // 第二个标志位
                    w.WriteBits(mappedLead, 3)
                    prevMappedLead <- mappedLead
                    let significantBits = 64 - (BitUtil.unmapLeadingZeros (int mappedLead))
                    w.WriteBits(xorv, significantBits)

        prev <- cur

    w.ToArray()

// =======================================================
// CHIMP Decompression Algorithm
// =======================================================

let decompress (data: uint64[]) (count: int) : double[] =
    if data = null || data.Length = 0 then
        raise (ArgumentException("Compressed data cannot be null or empty"))

    if count <= 0 then
        raise (ArgumentException("Count must be positive"))

    let r = BitReader(data)
    let output = Array.zeroCreate<double> count
    let mutable prev = 0UL
    let mutable prevMappedLeadNum = 0

    try
        for i = 0 to count - 1 do
            let cur =
                if i = 0 then
                    r.ReadBits(64)
                else
                    let firstFlag = r.ReadBit()
                    let secondFlag = r.ReadBit()
                    // Console.WriteLine($"{firstFlag} {secondFlag}")
                    if firstFlag = 0 then
                        if secondFlag = 0 then
                            prev
                        else
                            let leadNum = (int) (r.ReadBits(3))
                            let lead = BitUtil.unmapLeadingZeros (leadNum)
                            prevMappedLeadNum <- leadNum
                            let valid = (int) (r.ReadBits(6))
                            let trail = 64 - valid - lead
                            let xorv = r.ReadBits(valid)
                            let value = xorv <<< trail
                            value ^^^ prev
                    else if secondFlag = 0 then
                        let lead = BitUtil.unmapLeadingZeros (prevMappedLeadNum)
                        let valid = 64 - lead
                        let xorv = r.ReadBits(valid)
                        let value = xorv
                        value ^^^ prev
                    else
                        let leadNum = (int) (r.ReadBits(3))
                        prevMappedLeadNum <- leadNum
                        let lead = BitUtil.unmapLeadingZeros (leadNum)
                        let valid = 64 - lead
                        let xorv = r.ReadBits(valid)
                        let value = xorv
                        value ^^^ prev

            prev <- cur
            output[i] <- BitUtil.u2d cur

        output
    with ex ->
        printfn "Decompression error: %s" ex.Message
        reraise ()
