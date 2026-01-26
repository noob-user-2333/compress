module Compress.tANS

open System
open System.Linq
open Compress.tANS.Decoder
open Compress.tANS.encoder
open Compress.toolClass

type private eTableItem =
   struct
       val symbol: int
       val readBits: int
       val newX: int
       new (sym:int,readBits:int,newX:int) = {symbol = sym;readBits = readBits;newX = newX}
   end   

//核心功能
//传入连续的控制字段，每隔一定次数进行一次表重建
//根据表输出编码对应bit流
//根据bit流输出对应编码
//由于为控制字段，故默认为整数值
//仅需传入用于初始化的频率分布数组
type tANS(freqArray:int array,fieldLen:int) =

    let spreadArray = Array.zeroCreate<int> (freqArray.Sum())
    let encoder = Encoder(freqArray,spreadArray,fieldLen)
    let decoder = Decoder(freqArray,spreadArray,fieldLen)
    let fieldLen = fieldLen
    let mutable eStatus = 0
    let mutable dStatus = 0
    let fastSpread (freqArray:int array)(spreadArray:int array)(step: int) =
        let mutable pos = 0
        //获取每个符号发生的频次
        for i = 0 to freqArray.Length - 1 do
            let times = freqArray[i]
            //将其均匀扩散到spreadArray
            for time = 1 to times do
                spreadArray[pos] <- i
                pos <- (pos + step) % spreadArray.Length

    let update(freqArray:int array) =
        if freqArray.Sum() <> spreadArray.Length then
            raise (ArgumentException($"请确保每次传入的累计符号频次等于{spreadArray.Length}"))
        fastSpread freqArray spreadArray (spreadArray.Length * 8 / 5 + 3)
        decoder.Update(freqArray,spreadArray)
        encoder.Update(freqArray,spreadArray)

    member this.Encode(controlField: int,w:BitWriter) =
        eStatus <- encoder.Encode(eStatus,controlField,w)
    member this.Decode(r:BitReader) =
        let status,sym = decoder.Decode(dStatus,r)
        dStatus <- status
        sym

    member this.Update(freqArray:int array) = 
        update freqArray