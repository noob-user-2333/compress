module Compress.toolClass

open System
open System.Collections.Generic
open Microsoft.FSharp.NativeInterop

type PtrBuffer = 
    val Addr: nativeint
    val Length: int
    new(p: voidptr, len: int) = {  Addr =  NativePtr.toNativeInt(NativePtr.ofVoidPtr<byte>(p)); Length = len }
    member this.AsSpan()  =
        // this.Addr.AsReadOnly()
        ReadOnlySpan<double>(NativePtr.toVoidPtr (NativePtr.ofNativeInt<double> this.Addr), this.Length)
let lowBitMask =
    [| 0UL
       1UL
       3UL
       7UL
       15UL
       31UL
       63UL
       127UL
       255UL
       511UL
       1023UL
       2047UL
       4095UL
       8191UL
       16383UL
       32767UL
       65535UL
       131071UL
       262143UL
       524287UL
       1048575UL
       2097151UL
       4194303UL
       8388607UL
       16777215UL
       33554431UL
       67108863UL
       134217727UL
       268435455UL
       536870911UL
       1073741823UL
       2147483647UL
       4294967295UL
       8589934591UL
       17179869183UL
       34359738367UL
       68719476735UL
       137438953471UL
       274877906943UL
       549755813887UL
       1099511627775UL
       2199023255551UL
       4398046511103UL
       8796093022207UL
       17592186044415UL
       35184372088831UL
       70368744177663UL
       140737488355327UL
       281474976710655UL
       562949953421311UL
       1125899906842623UL
       2251799813685247UL
       4503599627370495UL
       9007199254740991UL
       18014398509481983UL
       36028797018963967UL
       72057594037927935UL
       144115188075855871UL
       288230376151711743UL
       576460752303423487UL
       1152921504606846975UL
       2305843009213693951UL
       4611686018427387903UL
       9223372036854775807UL
       18446744073709551615UL |]

// =======================================================
// Bit Writer - 带边界检查的增强版本
// =======================================================

type BitWriter() =
    let buffer = ResizeArray<uint64>(1024 * 1024)
    let mutable haveToArray = false
    let mutable accPos = 0
    let mutable acc = 0UL

    [<Literal>]
    let accPosMask = 0x3F

    member this.WriteBit(b: int) =
        let v = (uint64) b
        let value = (v &&& 1UL) <<< accPos
        acc <- acc ||| value
        accPos <- accPos + 1

        if accPos = 64 then
            buffer.Add(acc)
            acc <- 0UL
            accPos <- 0

    member this.WriteBits(v: uint64, n: int) =
        if n < 1 || n > 64 then
            raise (ArgumentException("Bit count must be between 1 and 64"))

        let value = v &&& lowBitMask[n]
        //需要分别写入两个字
        //因为value经过处理确保高位为0
        //故可以直接将value进行处理后和acc或
        let lowCount = 64 - accPos
        let low = acc ||| (value <<< accPos)
        acc <- low
        accPos <- (accPos + n)

        if accPos >= 64 then
            accPos <- accPos &&& accPosMask
            acc <- (value >>> lowCount) &&& lowBitMask[accPos]
            buffer.Add(low)


    member this.ToArray() =
        if haveToArray = false then
            let pos = buffer.Count * 64

            if accPos > 0 then
                buffer.Add(acc)

            haveToArray <- true
            struct(pos + accPos, buffer.ToArray())
        else
            raise (Exception("单个writer仅能调用一次ToArray"))

    member this.Clear() =
        haveToArray <- false
        accPos <- 0
        acc <- 0UL
        buffer.Clear()


// =======================================================
// Bit Reader - 带边界检查的安全版本
// =======================================================

type BitReader(data: uint64[]) =
    let data = data
    let mutable pos = 0
    let mutable acc = 0UL
    let mutable accPos = 0

    [<Literal>]
    let accPosMask = 0x3F

    member this.ReadBit() =
        if accPos = 0 then
            acc <- data[pos]
            pos <- pos + 1

        let ret = int (acc &&& 1UL)
        acc <- acc >>> 1
        accPos <- (accPos + 1) &&& accPosMask
        ret

    member this.ReadBits(n: int) =
        if n < 1 || n > 64 then
            raise (ArgumentException("Bit count must be between 1 and 64"))
        //需要分别读入两个字
        let result =
            if n + accPos > 64 then
                let lowCount = 64 - accPos
                let highCount = n - lowCount
                let low = acc
                acc <- data[pos]
                pos <- pos + 1
                accPos <- highCount
                let high = (acc &&& lowBitMask[highCount]) <<< lowCount
                acc <- acc >>> highCount
                high ||| low
            else
                if accPos = 0 then
                    acc <- data[pos]
                    pos <- pos + 1

                let v = (acc &&& lowBitMask[n])
                acc <- acc >>> n
                accPos <- (accPos + n) &&& accPosMask
                v

        result
    member this.UnreadBits(n: int) =
        if n > 0 then
            let globalPos =
                if accPos = 0 then pos * 64
                else (pos - 1) * 64 + accPos
    
            let newPos = globalPos - n
            let wordIndex = newPos / 64
            let bitOffset = newPos % 64
    
            if bitOffset = 0 then
                pos <- wordIndex
                acc <- 0UL
                accPos <- 0
            else
                pos <- wordIndex + 1
                acc <- data[wordIndex] >>> bitOffset
                accPos <- bitOffset
    

module BitUtil =
    let inline lz (x: uint64) =
        System.Numerics.BitOperations.LeadingZeroCount x

    let inline tz (x: uint64) =
        System.Numerics.BitOperations.TrailingZeroCount x

    let inline d2u (v: double) = BitConverter.DoubleToUInt64Bits v
    let inline u2d (v: uint64) = BitConverter.UInt64BitsToDouble v

    let inline extractMSB14 (x: uint64) =
        let msb14Mask = 0x7FFF000000000000UL // 14位高位掩码
        (x &&& msb14Mask) >>> (64 - 14) // 右移至最低位，保留14位有效特征
[<Interface>]
type ICompressor =
    abstract member Compress: BitWriter * PtrBuffer ->  struct(int*uint64 array)
    abstract member Decompress: BitReader * int -> double array
    abstract member GetName: unit -> string
