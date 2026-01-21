module Compress.toolClass

open System
let lowBitMask = 
        [| 0UL; 1UL; 3UL; 7UL; 15UL; 31UL; 63UL; 127UL; 255UL; 511UL; 1023UL; 2047UL; 4095UL; 8191UL; 16383UL; 32767UL; 65535UL; 131071UL; 262143UL; 524287UL; 1048575UL; 2097151UL; 4194303UL; 8388607UL; 16777215UL; 33554431UL; 67108863UL; 134217727UL; 268435455UL; 536870911UL; 1073741823UL; 2147483647UL; 4294967295UL; 8589934591UL; 17179869183UL; 34359738367UL; 68719476735UL; 137438953471UL; 274877906943UL; 549755813887UL; 1099511627775UL; 2199023255551UL; 4398046511103UL; 8796093022207UL; 17592186044415UL; 35184372088831UL; 70368744177663UL; 140737488355327UL; 281474976710655UL; 562949953421311UL; 1125899906842623UL; 2251799813685247UL; 4503599627370495UL; 9007199254740991UL; 18014398509481983UL; 36028797018963967UL; 72057594037927935UL; 144115188075855871UL; 288230376151711743UL; 576460752303423487UL; 1152921504606846975UL; 2305843009213693951UL; 4611686018427387903UL; 9223372036854775807UL; 18446744073709551615UL |]

// =======================================================
// Bit Writer - 带边界检查的增强版本
// =======================================================

type BitWriter() =
    let buffer = ResizeArray<uint64>(1024 * 1024)
    let mutable haveToArray = false
    let mutable accBits = 0
    let mutable acc = 0UL

    member this.WriteBit(b: int) =
        let v = (uint64)b    
        let value = (v &&& 1UL)  <<< accBits
        acc <- acc ||| value
        accBits <- accBits + 1
        if accBits = 64 then
            buffer.Add(acc)
            acc <- 0UL
            accBits <- 0

    member this.WriteBits(v: uint64, n: int) =
        if n < 1 || n > 64 then
            raise (ArgumentException("Bit count must be between 1 and 64"))           
        let value = v &&& lowBitMask[n]
        //需要分别写入两个字
        if n + accBits >= 64 then
            let lowCount = 64 - accBits
            let low  = acc + (value <<< accBits)
            acc <- (value >>> lowCount) &&& lowBitMask[accBits]
            accBits <- accBits + n
            accBits <- accBits % 64
            buffer.Add(low)
        else
            let v = (value <<< accBits)
            acc <- acc + v
            accBits <- accBits + n
            
        
    member this.ToArray() =
        if haveToArray = false then
            if accBits > 0 then
                buffer.Add(acc)
            haveToArray <- true    
            buffer.ToArray()            
        else
            raise (Exception("单个writer仅能调用一次ToArray"))
    member this.Clear() =
        haveToArray <- false
        accBits <- 0
        acc <- 0UL
        buffer.Clear()


// =======================================================
// Bit Reader - 带边界检查的安全版本
// =======================================================

type BitReader(data: uint64[]) =
    let data =
        if data = null || data.Length = 0 then
            raise (ArgumentException("data"))
        else
            data
    let mutable pos = 0
    let mutable acc = data[0]
    let mutable accPos = 0

    member this.ReadBit() =
        let ret =int ((acc >>> accPos) &&& 1UL)
        accPos <- accPos + 1
        if accPos = 64 then
            pos <- pos + 1
            acc <- data[pos]
            accPos <- 0
        ret
    member this.ReadBits(n: int) =
        if n < 1 || n > 64 then
            raise (ArgumentException("Bit count must be between 1 and 64"))
        //需要分别读入两个字
        let result =
                if n + accPos > 64 then
                    let lowCount = 64 - accPos
                    let highCount = n - lowCount
                    let low = acc >>> accPos
                    pos <- pos + 1
                    acc <- data[pos]
                    accPos <- highCount
                    let high = (acc &&& lowBitMask[highCount]) <<< lowCount
                    high + low
                else
                    let v = (acc >>> accPos)
                    accPos <- accPos + n
                    if accPos = 64 then
                        pos <- pos + 1
                        acc <- data[pos]
                        accPos <- 0
                    v &&& lowBitMask[n]
        result
module BitUtil =
    let inline lz (x: uint64) =
        System.Numerics.BitOperations.LeadingZeroCount x

    let inline tz (x: uint64) =
        System.Numerics.BitOperations.TrailingZeroCount x

    let inline d2u (v: double) = BitConverter.DoubleToUInt64Bits v
    let inline u2d (v: uint64) = BitConverter.UInt64BitsToDouble v
