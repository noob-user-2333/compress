module Compress.tANS

open System
open System.Linq

[<Literal>]
let updateInterval = 1024

[<Struct>]
type private decodeTableItem =
    { status: int
      symbol: int
      symBits: int
      readBits: int
      nextStatus: int }


//核心功能
//传入连续的控制字段，每隔一定次数进行一次表重建
//根据表输出编码对应bit流
//根据bit流输出对应编码
//由于为控制字段，故默认为整数值
//仅需传入用于初始化的频率分布数组
type tANS(initFreqArray: int array, fieldLen: int) =
    let initArray =
        if initFreqArray.Length <> (1 <<< fieldLen) || ((fieldLen - 1) &&& fieldLen) <> 0 then
            raise (ArgumentException("控制字段状态数量必须为2的控制字段bit数次"))
        else
            initFreqArray

    let initInterval =
        if initArray.Sum() < updateInterval then
            initArray.Sum()
        else
            raise (ArgumentException("初始频次不得超过更新间隔"))

    let freqArray = Array.copy initArray
    let spreadArray = Array.zeroCreate<int> (updateInterval)
    let fieldLen = fieldLen

    let fastSpread (step: int) =
        let mutable pos = 0
        //获取每个符号发生的频次
        for i = 0 to freqArray.Length - 1 do
            let times = freqArray[i]
            //将其均匀扩散到spreadArray
            for time = 1 to times do
                spreadArray[pos] <- i
                pos <- (pos + step) % spreadArray.Length

    let getDecodingTable = 1
    member this.Encode(controlField: int) = 1

    member this.Decode(n: int) = 1
