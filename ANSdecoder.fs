module Compress.tANS.Decoder
open System
open System.Linq
open Compress.toolClass

type private Item =
        struct
            val symbol: int
            val readBits: int
            val newX: int
            new (sym:int,readBits:int,newX:int) = {symbol = sym;readBits = readBits;newX = newX}
        end
type Decoder(freqArray:int array,spreadArray:int array,fieldLen:int) = 
    let table = Array.zeroCreate<Item>(spreadArray.Length)
    static let  update(freqArray:int array)(spreadArray:int array)(table:Item array)(fieldLen:int) =
        if spreadArray.Length <> table.Length then
            raise (Exception($"请确保每次更新的符号频次总数固定为{table.Length}"))
        let next = freqArray
        let R = fieldLen
        for i = 0 to table.Length - 1 do
            let currentSym = spreadArray[i]
            let sym = currentSym
            let x_tmp = next[currentSym] 
            next[currentSym] <- next[currentSym] + 1
            let readBits = R - (int)(Math.Floor( Math.Log2 x_tmp ))
            let newX = (x_tmp <<< readBits) - spreadArray.Length
            table[i] <- Item(sym,readBits,newX)
    do
        update freqArray spreadArray table fieldLen

    member this.Decode(status: int,r:BitReader) =
        let item = table[status]
        let sym = item.symbol
        let needReadBits = item.readBits
        let bits = if needReadBits > 0 then
                    r.ReadBits(item.readBits)
                    else 0UL
        let nextStatus = item.newX + (int)bits
        (nextStatus,sym)
    member this.Update(freqArray:int array,spreadArray:int array) =
        update freqArray spreadArray table fieldLen
