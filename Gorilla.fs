module Compress.Gorilla

open System
open Compress.toolClass


/// 压缩 double 数组
let compress (data: double[]) : byte[] =
    if data.Length = 0 then
        invalidArg "data" "empty input"

    let writer = BitWriter()

    // 写入第一个值（完整 64 bit）
    let first = BitConverter.DoubleToUInt64Bits(data[0])
    writer.WriteBits(first, 64)

    let mutable prevValue = first
    let mutable prevLeadingZeros = BitUtil.lz first
    let mutable prevTrailingZeros = BitUtil.tz first

    for i = 1 to data.Length - 1 do
        let curr = BitConverter.DoubleToUInt64Bits(data[i])
        let xor = prevValue ^^^ curr

        if xor = 0UL then
            // 控制位: 0
            writer.WriteBit(0)
        else
            writer.WriteBit(1) // 第一个控制位表示xor不为0

            let leadingZeros = BitUtil.lz (xor)
            let trailingZeros = BitUtil.tz (xor)
            let significantBits = 64 - leadingZeros - trailingZeros

            if leadingZeros >= prevLeadingZeros && trailingZeros >= prevTrailingZeros then
                // 控制位: 10 (重用之前的窗口)
                writer.WriteBit(0)
                // 窗口中的有效位数
                let windowBits = 64 - prevLeadingZeros - prevTrailingZeros

                // 写入窗口中的有效位
                // 将xor值右移prevTrailingZeros位，然后取出低windowBits位
                let value = (xor >>> prevTrailingZeros)
                writer.WriteBits(value, windowBits)
            else
                // 控制位: 11 (新窗口)
                writer.WriteBit(1)
                // 写入前导零个数(5 bits)和中心位数(6 bits)
                writer.WriteBits(uint64 leadingZeros, 5)
                writer.WriteBits(uint64 (significantBits - 1), 6)

                // 写入有意义的中心位
                let value = (xor >>> trailingZeros)
                writer.WriteBits(value, significantBits)

                // 更新窗口
                prevLeadingZeros <- leadingZeros
                prevTrailingZeros <- trailingZeros

        prevValue <- curr

    writer.ToArray()


/// 解压缩
let decompress (bytes: byte[]) (length: int) : double[] =
    if length = 0 then
        invalidArg "length" "zero length"

    let result = Array.zeroCreate<double> length
    let r = BitReader(bytes)
    // 读取第一个值
    let first = r.ReadBits 64
    result[0] <- BitConverter.UInt64BitsToDouble(first)

    let mutable prevValue = first
    let mutable prevLeadingZeros = BitUtil.lz first
    let mutable prevTrailingZeros = BitUtil.tz first

    for i = 1 to length - 1 do
        let isZeroXor = r.ReadBit() = 0

        if isZeroXor then
            // xor = 0, 值与前一个相同
            result[i] <- BitConverter.UInt64BitsToDouble(prevValue)
        else
            let reuseWindow = r.ReadBit() = 0

            let xorValue =
                if reuseWindow then
                    // 重用窗口 (10)
                    let windowBits = 64 - prevLeadingZeros - prevTrailingZeros
                    let value = r.ReadBits windowBits
                    value <<< prevTrailingZeros
                else
                    // 新窗口 (11)
                    let leading = int (r.ReadBits 5)
                    let leadingZeros = if leading < 32 then leading else 31
                    let significantBits = int (r.ReadBits 6) + 1
                    let trailingZeros = 64 - leadingZeros - significantBits

                    // 更新窗口
                    prevLeadingZeros <- leadingZeros
                    prevTrailingZeros <- trailingZeros

                    let value = r.ReadBits significantBits
                    value <<< trailingZeros

            let currentValue = prevValue ^^^ xorValue
            result[i] <- BitConverter.UInt64BitsToDouble(currentValue)
            prevValue <- currentValue

    result
