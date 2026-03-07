module Compress.Elf

open System
open Compress.toolClass
let private pow10 =
        [| for i in 0 .. 20 -> Math.Pow(10.0, float i) |]
let private pow10N =         
        [| for i in 0 .. 20 -> Math.Pow(10.0, float -i) |]
let private fTable =
    [| 0; 4; 7; 10; 14; 17; 20; 24; 27; 30; 34; 37; 40; 44; 47; 50; 54; 57; 60; 64; 67 |]

let private getFAlpha (alpha: int) =
    if alpha < fTable.Length then fTable.[alpha]
    else int (Math.Ceiling(float alpha * (Math.Log(10.0) / Math.Log(2.0))))     
let private getSignificantCount (v: float) (sp: int) : int =
        let mutable i = if sp >= 0 then 1 else -sp
        let mutable temp = v * pow10[i]
        // (long)temp == temp 等价于 truncate temp = temp
        // 即 temp 没有小数部分
        while float (int64 temp) <> temp do
            i <- i + 1
            temp <- v * pow10[i]
        // 正确性校验：回除验证，防止高有效数字的精度问题
        // if temp / pow10[i] <> v then 17
        sp + i + 1
/// 获取 α 和 β*（完全对应官方 Java 的 getAlphaAndBetaStar）
let private getAlphaAndBetaStar (v: float) : struct (int * int) =
        let av = abs v
        let log10v = Math.Log10(av)
        let sp = int (Math.Floor(log10v))
        let beta = getSignificantCount av sp
        let alpha = beta - sp - 1
        // v = 10^(-i) 的判断：v < 1 且 sp 恰好等于 log10(v)（即 log10v 是整数）
        let betaStar =
            if av < 1.0 && float sp = log10v then 0
            else beta
        struct (alpha, betaStar)
let private eraseTrailZero(v:float) =
    let struct (alpha,betaStar) = getAlphaAndBetaStar v
    let vLong = BitUtil.d2u v
    let e = ( (vLong >>> 52)) &&& 0x7FFUL
    let fAlpha = getFAlpha alpha
    let gAlpha = (uint64 fAlpha) + e - 1023UL
    let eraseBits = 52UL - gAlpha
    let mask = 0xFFFFFFFFFFFFFFFFUL <<< int eraseBits
    let delta = (~~~mask) &&& vLong
    let doErase = betaStar < 16 && delta <> 0UL && eraseBits > 4UL
    let vPrimeLong = if doErase then mask &&& vLong else vLong
    struct (doErase,betaStar,vPrimeLong)
let private recoveryTrailZero(vPrime:float)(betaStar:int)=
    if betaStar = 0 then
        let sp = int (Math.Floor(Math.Log10(abs vPrime)))
        let recovered =
            let r = pow10N[-sp - 1]
            if vPrime < 0.0 then -r else r
        recovered
    else
        let sp = int (Math.Floor(Math.Log10(abs vPrime)))
        let alpha = betaStar - sp - 1
        let scale = pow10[alpha]
        let scaled = vPrime * scale
        let recovered = if vPrime < 0.0 then floor scaled / scale  else ceil scaled / scale  
        recovered
let private leadingZerosMap : uint64[] = [|
        // 0-7: 0UL
        0UL;0UL;0UL;0UL;0UL;0UL;0UL;0UL;
        // 8-11: 1UL
        1UL;1UL;1UL;1UL;
        // 12-15: 2UL
        2UL;2UL;2UL;2UL;
        // 16-17: 3UL
        3UL;3UL;
        // 18-19: 4UL
        4UL;4UL;
        // 20-21: 5UL
        5UL;5UL;
        // 22-23: 6UL
        6UL;6UL;
        // 24-64: 7UL（共41个元素：64-24+1=41）
        yield! Array.create 41 7UL
    |]
let private leadingZerosUnMap  = [|0;8;12;16;18;20;22;24|]       
/// 压缩 double 数组
let compress (writer: BitWriter) (data: double[]) =
    if data.Length = 0 then
        invalidArg "data" "empty input"
    // 写入第一个值
    let struct(doErase,betaStar,v) = eraseTrailZero data[0]
    let mutable lastTz = BitUtil.tz v
    let mutable lastControlLz = leadingZerosMap[BitUtil.lz v]
    let validBit = 64 - lastTz
    let mutable last = v
    if doErase then
        let controlBit = 1 ||| (betaStar <<< 1) ||| (lastTz <<< 5)
        writer.WriteBits(uint64 controlBit,11)
    else
        let controlBit = lastTz <<< 1
        writer.WriteBits(uint64 controlBit,7)
    writer.WriteBits(v >>> lastTz,validBit)
    //开始写入后续字节    
    for i = 1 to data.Length - 1 do
        let struct(doErase,betaStar,v) = eraseTrailZero data[i]
        let xor = v ^^^ last
        let lz = BitUtil.lz xor
        let tz = BitUtil.tz xor
        if doErase then
            let control = 1 ||| (betaStar <<< 1)
            writer.WriteBits(uint64 control,5)
        else
            writer.WriteBit(0)
        //先确定编码模式
        let controlLz = leadingZerosMap[lz]
        let curLz = leadingZerosUnMap[int controlLz]
        //01编码模式
        if xor = 0UL then
            writer.WriteBits(1UL,2)
        else
            //00编码模式
            if controlLz = lastControlLz && tz >= lastTz then
                let validBit = 64 - curLz - lastTz
                let value = (xor >>> (lastTz)) <<< 2
                writer.WriteBits(value,validBit + 2)
            else
                let validBit = 64 - curLz - tz
                //10编码模式
                if validBit <= 16 then
                    let control = 2UL ||| (controlLz <<< 2) ||| uint64 ((validBit - 1) <<< 5)
                    writer.WriteBits(control,9)
                else//11编码模式
                    let control = 3UL ||| (controlLz <<< 2) ||| uint64 ((validBit - 1) <<< 5)
                    writer.WriteBits(control,11)
                let value = xor >>> tz
                writer.WriteBits(value,validBit)
        lastTz <- tz
        lastControlLz <- controlLz
        last <- v
    writer.ToArray()


/// 解压缩
let decompress (bytes: uint64[]) (length: int) : double[] =
    if length = 0 then
        invalidArg "length" "zero length"
    let result = Array.zeroCreate<double> length
    let r = BitReader(bytes)
    // 读取第一个值
    let doErase = r.ReadBit()
    let mutable lastTz = 0
    let mutable lastControlLz = 0UL
    let mutable last = 0UL
    
    if doErase = 1 then
        let betaStar = r.ReadBits(4)
        let tz = r.ReadBits(6)
        let validBit = 64UL - tz
        let v = (r.ReadBits(int validBit)) <<< int tz
        let oriValue = recoveryTrailZero(BitUtil.u2d(v))(int betaStar)
        last <- v
        lastTz <- int tz
        lastControlLz <- leadingZerosMap[int(BitUtil.lz v)]
        result[0] <- oriValue
    else
        let tz = r.ReadBits(6)
        let validBit = 64UL - tz
        let v = (r.ReadBits(int validBit)) <<< int tz
        last <- v
        lastTz <- int tz
        lastControlLz <- leadingZerosMap[int(BitUtil.lz v)]
        result[0] <- BitUtil.u2d v
    //读取后续数值    
    for i = 1 to length - 1 do
        let doErase = r.ReadBit()
        let betaStar = if doErase = 1 then r.ReadBits(4) else 0UL
        let mode = r.ReadBits(2)
        let xor = 
            match mode with
                | 0UL -> //00模式 复用上一个数的尾数0和前导0
                    let validBit = 64 - lastTz - leadingZerosUnMap[int lastControlLz]
                    let value = r.ReadBits(validBit) <<< lastTz
                    lastTz <- BitUtil.tz value
                    value
                | 1UL ->  //01模式，直接复用上一个数
                    lastControlLz <- 7UL
                    lastTz <- 64
                    0UL
                | 2UL -> //10模式 有效位不大于16
                    let control = r.ReadBits(7)
                    let controlLead = control &&& 7UL
                    let lead = leadingZerosUnMap[int controlLead]
                    let validBit = (control >>> 3) + 1UL
                    let trail = 64 - int validBit -  lead
                    lastTz <- trail
                    lastControlLz <- controlLead
                    let validValue = r.ReadBits(int validBit) <<< trail
                    validValue
                | 3UL -> //11模式 有效位大于16
                    let control = r.ReadBits(9)
                    let controlLead = control &&& 7UL
                    let lead = leadingZerosUnMap[int controlLead]
                    let validBit = (control >>> 3) + 1UL
                    let trail = 64 - int validBit -  lead
                    lastTz <- trail
                    lastControlLz <- controlLead
                    let validValue = r.ReadBits(int validBit) <<< trail
                    validValue
                | _ -> raise(Exception("不应当存在走入该分支的可能性"))
        let v = xor ^^^ last
        last <- v
        //开始擦除
        let value = BitUtil.u2d(v)
        if doErase = 1 then
            result[i] <- recoveryTrailZero(value)(int betaStar)
        else
            result[i] <- value
    result
