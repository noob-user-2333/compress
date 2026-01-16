module Compress.toolClass

open System
open System.Runtime.InteropServices
// [<Literal>]
let lowBitMask = 
        [| 0UL; 1UL; 3UL; 7UL; 15UL; 31UL; 63UL; 127UL; 255UL; 511UL; 1023UL; 2047UL; 4095UL; 8191UL; 16383UL; 32767UL; 65535UL; 131071UL; 262143UL; 524287UL; 1048575UL; 2097151UL; 4194303UL; 8388607UL; 16777215UL; 33554431UL; 67108863UL; 134217727UL; 268435455UL; 536870911UL; 1073741823UL; 2147483647UL; 4294967295UL; 8589934591UL; 17179869183UL; 34359738367UL; 68719476735UL; 137438953471UL; 274877906943UL; 549755813887UL; 1099511627775UL; 2199023255551UL; 4398046511103UL; 8796093022207UL; 17592186044415UL; 35184372088831UL; 70368744177663UL; 140737488355327UL; 281474976710655UL; 562949953421311UL; 1125899906842623UL; 2251799813685247UL; 4503599627370495UL; 9007199254740991UL; 18014398509481983UL; 36028797018963967UL; 72057594037927935UL; 144115188075855871UL; 288230376151711743UL; 576460752303423487UL; 1152921504606846975UL; 2305843009213693951UL; 4611686018427387903UL; 9223372036854775807UL; 18446744073709551615UL |]
    
// let lowBitMask = [|
//                        for i = 0 to 64 do
//                            if i = 64 then
//                                System.UInt64.MaxValue
//                            else
//                                if i = 0 then
//                                    0UL
//                                else    
//                                    (1UL <<< i) - 1UL
//                        |]
// =======================================================
// Bit Writer - 带边界检查的增强版本
// =======================================================

type BitWriter() =
    let buffer = ResizeArray<uint64>()
    let mutable pos = 0

    member this.WriteBit(b: int) =
        let offset = pos / 64
        let bitOffset = pos % 64
        if offset = buffer.Count then
            buffer.Add(0UL)
        let v = (uint64)b    
        // if b = 1 then
        let value = (v &&& 1UL)  <<< bitOffset
        buffer[offset] <- buffer[offset] ||| value
        pos <- pos + 1

    member this.WriteBits(v: uint64, n: int) =
        if n < 1 || n > 64 then
            raise (ArgumentException("Bit count must be between 1 and 64"))           
        let offset = pos / 64
        let bitOffset = pos % 64
        let value = v &&& lowBitMask[n]
        if offset = buffer.Count then
            buffer.Add(0UL)
        //需要分别写入两个字
        if n + bitOffset > 64 then
            let lowCount = 64 - bitOffset
            let highCount = n - lowCount
            let low = buffer[offset] + (value <<< bitOffset)
            let high = value >>> lowCount
            buffer[offset] <- low
            buffer.Add(high)
        else
            let low = buffer[offset] + (value <<< bitOffset)
            buffer[offset] <- low
        pos <- pos + n    
    member this.ToArray() =
        buffer.ToArray()



// =======================================================
// Bit Reader - 带边界检查的安全版本
// =======================================================

type BitReader(data: uint64[]) =
    let data =
        if data = null || data.Length = 0 then
            raise (ArgumentException("data"))
        else
            data

    let totalBytes = data.Length * 8
    let maxPos = totalBytes * 8
    let mutable pos = 0

    member this.ReadBit() =
        if pos >= maxPos then
            raise (Exception("no more data"))

        let byteOffset = pos / 64
        let bitOffset = pos % 64
        pos <- pos + 1
        int ((data[byteOffset] >>> bitOffset) &&& 1UL)

    member this.ReadBits(n: int) =
        if n < 1 || n > 64 then
            raise (ArgumentException("Bit count must be between 1 and 64"))
        let offset = pos / 64
        let bitOffset = pos % 64
        //需要分别读入两个字
        let result =
                if n + bitOffset > 64 then
                    let lowCount = 64 - bitOffset
                    let highCount = n - lowCount
                    let low = data[offset] >>> bitOffset
                    let high = (data[offset + 1] &&& lowBitMask[highCount]) <<< lowCount
                    high + low
                else
                    (data[offset] >>> bitOffset) &&& lowBitMask[n]
        pos <- pos + n
        result

module BitUtil =
    let inline lz (x: uint64) =
        System.Numerics.BitOperations.LeadingZeroCount x

    let inline tz (x: uint64) =
        System.Numerics.BitOperations.TrailingZeroCount x

    let inline d2u (v: double) = BitConverter.DoubleToUInt64Bits v
    let inline u2d (v: uint64) = BitConverter.UInt64BitsToDouble v
