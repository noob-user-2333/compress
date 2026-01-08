module Compress.toolClass

open System


// =======================================================
// Bit Writer - 带边界检查的增强版本
// =======================================================

type BitWriter() =
    let buffer = ResizeArray<byte>(1024)
    let byteOffsetMask = 0x7FFFFFF8
    let bitOffsetMask = 0x7
    let mutable pos = 0

    member this.WriteBit(b: int) =
        let byteOffset = (pos &&& byteOffsetMask)  >>> 3
        let bitOffset = pos &&& bitOffsetMask
        if byteOffset = buffer.Count then
            buffer.Add(0uy)
        if b = 1 then
            let value = 1uy <<< bitOffset
            buffer[byteOffset] <- buffer[byteOffset] ||| value
        pos <- pos + 1

    member this.WriteBits(v: uint64, n: int) =
        if n < 1 || n > 64 then
            raise (ArgumentException("Bit count must be between 1 and 64"))
        for i = 0 to n - 1 do
            this.WriteBit(int ((v >>> i) &&& 1UL))

    member this.ToArray() =
        buffer.ToArray()



// =======================================================
// Bit Reader - 带边界检查的安全版本
// =======================================================

type BitReader(data: byte[]) =
    let data = if data = null then raise (ArgumentException("data")) else data
    let totalBytes = data.Length
    let maxPos = totalBytes * 8
    let byteOffsetMask = 0x7FFFFFF8
    let bitOffsetMask = 0x7
    let mutable pos = 0
    
    member this.ReadBit() =
        if pos >= maxPos then
            raise (Exception("no more data"))
        let byteOffset = (pos &&& byteOffsetMask)  >>> 3
        let bitOffset = pos &&& bitOffsetMask
        pos <- pos + 1
        int ((data[byteOffset] >>> bitOffset) &&& 1uy)

    member this.ReadBits(n: int) =
        if n < 1 || n > 64 then
            raise (ArgumentException("Bit count must be between 1 and 64"))
        let mutable v = 0UL
        for time = 0 to n - 1 do
            v <- v ||| ((uint64 (this.ReadBit())) <<< time)
        v

module BitUtil =
    let inline lz (x: uint64) =
            System.Numerics.BitOperations.LeadingZeroCount x

    let inline tz (x: uint64) =
            System.Numerics.BitOperations.TrailingZeroCount x

    let d2u (v: double) = BitConverter.DoubleToUInt64Bits v
    let u2d (v: uint64) = BitConverter.UInt64BitsToDouble v