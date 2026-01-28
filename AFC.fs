module Compress.AFC

open Compress.toolClass

type PDFCM() =
    let predictTable = Array.zeroCreate (256)
    let mutable prev = 0.0
    let mutable prevNum = 0UL
    let mutable hash = 0
    let lowWordMask = 0xFFFFFFFFFFFFFUL
    let highWordMask = 0xFFF0000000000000UL

    let updateHash (delta: double) =
        let deltaNum = BitUtil.d2u delta
        let deltaHighWord = deltaNum >>> 40
        hash <- ((hash <<< 2) ^^^ (int) (deltaHighWord &&& 0xFFUL)) &&& 0xFF

    member this.update (value: double) (valueNum: uint64) =
        let actualDelta = value - prev
        predictTable[hash] <- actualDelta
        updateHash (actualDelta)
        prev <- value
        prevNum <- valueNum

    member this.predict() =
        let highWord = prevNum &&& highWordMask
        let predictDelta = predictTable[hash]
        let predict = prev + predictDelta
        let predictNum = BitUtil.d2u predict
        let result = highWord + (predictNum &&& lowWordMask)
        result

let compress (w: BitWriter) (values: double[])  =
    let p = PDFCM()
    let first = BitUtil.d2u values[0]
    p.update values[0] first
    w.WriteBits(first, 64)
    let mutable prev = first

    for i = 1 to values.Length - 1 do
        let value = BitUtil.d2u values[i]

        if value = prev then
            w.WriteBit(0)
        else
            //处理历史窗口
            //需要注意即使出现重复值
            //此时写入的offset是实际重复值相对当前值offset + 1
            let start = if i - 8 < 0 then 0 else i - 8
            let length = i - start
            let mutable repeatOffset = -1
            let mutable wMaxZeroOffset = 0
            let mutable wMaxZeroByteCount = -1
            let mutable wMaxTrailZeroByteCount = 0
            let mutable wMaxXorv = 0UL

            for j = 0 to length - 1 do
                let index = start + j
                let oldValue = BitUtil.d2u values[index]
                //查找是否存在重复值
                if oldValue = value then
                    repeatOffset <- length - j
                //查找窗口残差
                let xorv = oldValue ^^^ value
                let lzByteCount = (BitUtil.lz xorv) / 8
                let tzByteCount = (BitUtil.tz xorv) / 8
                let zeroByteCount = lzByteCount + tzByteCount

                if zeroByteCount > wMaxZeroByteCount then
                    wMaxZeroByteCount <- zeroByteCount
                    wMaxZeroOffset <- length - j
                    wMaxTrailZeroByteCount <- tzByteCount
                    wMaxXorv <- xorv

            if repeatOffset > 0 then
                //因为从低位开始写入，须将正常写入数字进行倒换
                w.WriteBits(1UL, 2)
                w.WriteBits((uint64) (repeatOffset - 1), 3)
            else
                //确定窗口内都不存在相同值
                let predictValue = p.predict ()
                let pxorv = predictValue ^^^ value
                let pxorvLzByteCount = (BitUtil.lz pxorv) / 8
                let pxorvTzByteCount = (BitUtil.tz pxorv) / 8
                let pxorvZeroByteCount = pxorvLzByteCount + pxorvTzByteCount
                //如果预测值xor后前导和尾随0数量更多
                if pxorvZeroByteCount > wMaxZeroByteCount then
                    //因为从低位开始写入，须将正常写入数字进行倒换
                    //1110 -> 0111
                    w.WriteBits(7UL, 4)
                    //需要注意如果预测完全一致则
                    //pxorvTzByteCount validByteCount均为0
                    w.WriteBits((uint64) pxorvTzByteCount, 3)
                    let validByteCount = 8 - pxorvZeroByteCount

                    if validByteCount > 0 then
                        w.WriteBits((uint64) validByteCount, 3)
                        w.WriteBits(pxorv >>> (pxorvTzByteCount * 8), validByteCount * 8)
                    else
                        w.WriteBits(0UL, 3)
                //如果窗口残差0更多
                else if
                    //确定窗口残差0字节总数不为0
                    wMaxZeroByteCount > 0
                then
                    //因为从低位开始写入，须将正常写入数字进行倒换
                    //110 -> 011
                    w.WriteBits(3UL, 3)
                    w.WriteBits((uint64) (wMaxZeroOffset - 1), 3)
                    w.WriteBits((uint64) wMaxTrailZeroByteCount, 3)
                    let validByteCount = 8 - wMaxZeroByteCount
                    w.WriteBits((uint64) validByteCount, 3)
                    w.WriteBits(wMaxXorv >>> (wMaxTrailZeroByteCount * 8), validByteCount * 8)
                //否则只能进异常处理
                else
                    w.WriteBits(15UL, 4)
                    w.WriteBits(value, 64)

        prev <- value
        p.update values[i] (value)

    w.ToArray()

let decompress (data: uint64[]) (count: int) : double[] =
    let r = BitReader(data)
    let p = PDFCM()
    let result = Array.zeroCreate (count)
    let mutable prev = r.ReadBits(64)
    result[0] <- BitUtil.u2d (prev)
    p.update result[0] prev

    for i = 1 to count - 1 do
        let isSame = r.ReadBit() = 0

        let value =
            if isSame then
                prev
            else
                let windowsContain = r.ReadBit() = 0

                if windowsContain then
                    let offset = r.ReadBits(3) + 1UL
                    BitUtil.d2u result[i - (int) offset]
                else
                    //考虑窗口残差
                    let isWindowsR = r.ReadBit() = 0

                    if isWindowsR then
                        let offset = r.ReadBits(3) + 1UL
                        let tzByte = r.ReadBits(3)
                        let validByte = r.ReadBits(3)
                        let windowsR = r.ReadBits((int) (validByte * 8UL)) <<< (int ((tzByte * 8UL)))
                        windowsR ^^^ (BitUtil.d2u result[i - (int) offset])
                    else
                        //考虑预测或者异常
                        let isException = r.ReadBit() = 1

                        if isException then
                            r.ReadBits(64)
                        else
                            //需要使用预测器中的数值
                            let preNum = p.predict ()
                            let tzByte = r.ReadBits(3)
                            let validByte = r.ReadBits(3)
                            //如果预测完全准确
                            if tzByte = 0UL && validByte = 0UL then
                                preNum
                            else
                                let value = (r.ReadBits((int) validByte * 8)) <<< ((int) tzByte * 8)
                                value ^^^ preNum

        result[i] <- BitUtil.u2d value
        p.update result[i] value
        prev <- value

    result
