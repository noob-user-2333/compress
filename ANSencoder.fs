module Compress.tANS.encoder
open System
open System.Linq
open Compress.toolClass


type Encoder(freqArray:int array,spreadArray:int array,fieldLen:int) =
    let fieldStatusCount = 1 <<< fieldLen
    let k = Array.zeroCreate<int>(fieldStatusCount)
    let nb = Array.zeroCreate<int>(fieldStatusCount)
    let start = Array.zeroCreate<int>(fieldStatusCount)
    let table = Array.zeroCreate<int>(spreadArray.Length)
    let  update(freqArray:int array)(spreadArray:int array)(fieldLen:int) =
        if spreadArray.Length <> table.Length then
            raise (Exception($"请确保每次更新的符号频次总数固定为{table.Length}"))
        let R = fieldLen
        let r = R + 1
        for i = 0 to fieldStatusCount - 1 do
            k[i] <- R - (int)(Math.Floor(Math.Log2(freqArray[i])))
            nb[i] <- (k[i] <<< r) - (freqArray[i] <<< k[i])
            start[i] <- freqArray[0..i].Sum() - freqArray[i]
        let next = Array.copy freqArray
        for i = 0 to spreadArray.Length - 1 do
            let s = spreadArray[i]
            table[start[s] + next[s]] <- i + spreadArray.Length
            next[s] <- next[s] + 1
    do
        update freqArray spreadArray fieldLen

    member this.Encode(status: int,symbol:int,w:BitWriter) =
        let r = (int)(Math.Log2((float)table.Length * 2.0))
        let nbBits = (status + nb[symbol]) >>> r
        w.WriteBits((uint64)status,nbBits)
        table[start[symbol] + status]
        
    member this.Update(freqArray:int array,spreadArray:int array) =
        update freqArray spreadArray fieldLen

