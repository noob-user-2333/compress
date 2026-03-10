module Compress.ElfUtil

open System
open Compress.Chimp
open Compress.ZeroCountModel

let private pow10 = [| for i in 0..20 -> Math.Pow(10.0, float i) |]
let private pow10N = [| for i in 0..20 -> Math.Pow(10.0, float -i) |]

let private fTable =
    [| 0
       4
       7
       10
       14
       17
       20
       24
       27
       30
       34
       37
       40
       44
       47
       50
       54
       57
       60
       64
       67 |]

let private getFAlpha (alpha: int) =
    if alpha < fTable.Length then
        fTable.[alpha]
    else
        int (Math.Ceiling(float alpha * (Math.Log(10.0) / Math.Log(2.0))))

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
    let betaStar = if av < 1.0 && float sp = log10v then 0 else beta
    struct (alpha, betaStar)

let eraseTrailZero (v: float) =
    let struct (alpha, betaStar) = getAlphaAndBetaStar v
    let vLong = BitUtil.d2u v
    let e = ((vLong >>> 52)) &&& 0x7FFUL
    let fAlpha = getFAlpha alpha
    let gAlpha = (uint64 fAlpha) + e - 1023UL
    let eraseBits = 52UL - gAlpha
    let mask = 0xFFFFFFFFFFFFFFFFUL <<< int eraseBits
    let delta = (~~~mask) &&& vLong
    let doErase = betaStar < 16 && delta <> 0UL && eraseBits > 4UL
    let vPrimeLong = if doErase then mask &&& vLong else vLong
    struct (doErase, betaStar, vPrimeLong)

let recoveryTrailZero (vPrime: float) (betaStar: int) =
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

        let recovered =
            if vPrime < 0.0 then
                floor scaled / scale
            else
                ceil scaled / scale

        recovered

[<Literal>]
let leadBitCount = 3

[<Literal>]
let trailBitCount = 3

let zeroLeadFreqArray =
    [| for i = 0 to 64 do
           0 |]

let zeroTrailFreqArray =
    [| for i = 0 to 64 do
           0 |]

let zeroLeadModule = ZeroCountModel(leadBitCount)
let zeroTrailModule = ZeroCountModel(trailBitCount)
