module Compress.tANS

open System
open System.Linq
open System.Numerics
open System.Runtime.InteropServices
open Compress.toolClass
type private Item =
        struct
            val symbol: int
            val readBits: int
            val newX: int
            new (sym:int,readBits:int,newX:int) = {symbol = sym;readBits = readBits;newX = newX}
        end
type private Decoder(freqArray:int array,spreadArray:int array,fieldLen:int) = 
    let table = Array.zeroCreate<Item>(spreadArray.Length)
    static let update(freqArray:int array)(spreadArray:int array)(table:Item array) =
        if spreadArray.Length <> table.Length then
            raise (Exception($"请确保每次更新的符号频次总数固定为{table.Length}"))
        let next = Array.copy freqArray
        let R = BitOperations.Log2((uint32)spreadArray.Length)
        for i = 0 to table.Length - 1 do
            let currentSym = spreadArray[i]
            let sym = currentSym
            let x_tmp = next[currentSym] 
            next[currentSym] <- next[currentSym] + 1
            let readBits = R - BitOperations.Log2((uint32) x_tmp )
            let newX = (x_tmp <<< readBits) - spreadArray.Length
            table[i] <- Item(sym,readBits,newX)
    do
        update freqArray spreadArray table 

    member this.Decode(status: int,r:BitReader,[<Out>] nextStatus:byref<int>) =
        let item = table[status]
        let sym = item.symbol
        let needReadBits = item.readBits
        let bits = if needReadBits > 0 then
                    r.ReadBits(item.readBits)
                    else 0UL
        let newStatus = item.newX + (int)bits
        nextStatus <- newStatus
        sym
    member this.Update(freqArray:int array,spreadArray:int array) =
        update freqArray spreadArray table 

type private Encoder(freqArray:int array,spreadArray:int array,fieldLen:int) =
    let k = Array.zeroCreate<int>(freqArray.Length)
    let nb = Array.zeroCreate<int>(freqArray.Length)
    let start = Array.zeroCreate<int>(freqArray.Length)
    let table = Array.zeroCreate<int>(spreadArray.Length)
    let  update(freqArray:int array)(spreadArray:int array) =
        if spreadArray.Length <> table.Length then
            raise (Exception($"请确保每次更新的符号频次总数固定为{table.Length}"))
        let R = BitOperations.Log2((uint32)spreadArray.Length)
        let r = R + 1
        for i = 0 to freqArray.Length - 1 do
            k[i] <- R - BitOperations.Log2((uint32)freqArray[i])
            nb[i] <- (k[i] <<< r) - (freqArray[i] <<< k[i])
            let sum = if i > 0 then freqArray[0..i - 1].Sum() else 0
            start[i] <- sum - freqArray[i]
        let next = Array.copy freqArray
        for i = 0 to spreadArray.Length - 1 do
            let s = spreadArray[i]
            table[start[s] + next[s]] <- i + spreadArray.Length
            next[s] <- next[s] + 1
    do
        if 1 <<< fieldLen <> freqArray.Length then
            raise (ArgumentException("请确保传入符号频次数组长度为2^fieldLen"))
        if spreadArray.Length &&& (spreadArray.Length - 1)  > 0 then
            raise (ArgumentException("请确保传入的spreadArray长度等于2的幂"))
        update freqArray spreadArray 

    member this.Encode(status: int,symbol:int,w:BitWriter) =
        let r = BitOperations.Log2((uint32)table.Length) + 1
        let nbBits = (status + nb[symbol]) >>> r
        if nbBits > 0 then
            w.WriteBits((uint64)status,nbBits)
        let newStatus = status >>> nbBits    
        table[start[symbol] + newStatus]
        
    member this.Update(freqArray:int array,spreadArray:int array) =
        update freqArray spreadArray 
//核心功能
//传入连续的控制字段，每隔一定次数进行一次表重建
//根据表输出编码对应bit流
//根据bit流输出对应编码
//由于为控制字段，故默认为整数值
//仅需传入用于初始化的频率分布数组
type tANS(freqArray:int array,fieldLen:int) =
    let spreadArray = Array.zeroCreate<int> (freqArray.Sum())
    let step = (spreadArray.Length * 8 / 5 + 3)
    let posSeq = [|
        let mutable pos = -step
        for i = 1 to spreadArray.Length do
            pos <- (pos + step) % spreadArray.Length
            pos
    |]
    static let fastSpread (freqArray:int array)(spreadArray:int array)(posSeq: int array) =
        let mutable pos = 0
        //获取每个符号发生的频次
        for i = 0 to freqArray.Length - 1 do
            let times = freqArray[i]
            //将其均匀扩散到spreadArray
            for time = 1 to times do
                spreadArray[posSeq[pos]] <- i
                pos <- pos + 1
    do
        if (spreadArray.Length - 1) &&& spreadArray.Length > 0 then
            raise (ArgumentException("累计频次必须为2的幂"))
        if (freqArray.Length - 1) &&& freqArray.Length > 0 then
            raise (ArgumentException("频次数组长度必须为2的幂"))
        fastSpread freqArray spreadArray posSeq
    let encoder = Encoder(freqArray,spreadArray,fieldLen)
    let decoder = Decoder(freqArray,spreadArray,fieldLen)
    let fieldLen = fieldLen
    let mutable eStatus = spreadArray.Length - 3
    let mutable dStatus = eStatus
    

    let update(freqArray:int array) =
        if freqArray.Sum() <> spreadArray.Length then
            raise (ArgumentException($"请确保每次传入的累计符号频次等于{spreadArray.Length}"))
        fastSpread freqArray spreadArray posSeq
        decoder.Update(freqArray,spreadArray)
        encoder.Update(freqArray,spreadArray)

    member this.Encode(controlField: int,w:BitWriter) =
        eStatus <- encoder.Encode(eStatus,controlField,w)
    member this.Decode(r:BitReader) =
        let sym = decoder.Decode(dStatus,r,&dStatus)
        sym

    member this.Update(freqArray:int array) = 
        update freqArray