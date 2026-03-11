module Compress.IAACutil

open Compress.ElfUtil
open Compress.Huffman
open Compress.LUT

[<Literal>]
let evalThreshold = 1024 * 8

[<Literal>]
let fmin = 1

let  leadFreqArray =
    [| for i = 0 to 64 do
           0 |]

let  trailFreqArray =
    [| for i = 0 to 64 do
           0 |]

let  elfControlSym =
    [| for i = 0 to 16 do
           if i = 0 then 0 else (i - 1) * 2 + 1 |]

let  elfControlSymLen =
    [| for i = 0 to 16 do
           if i = 0 then 1 else 5 |]

let  erasedHuffman = Huffman.Huffman(elfControlSym, elfControlSymLen)

let  controlSym =
    [| yield 0
       yield 2
       yield!
           [| for leadControl = 0 to (1 <<< leadBitCount) - 1 do
                  for trailControl = 0 to (1 <<< trailBitCount) - 1 do
                      5 ||| (leadControl <<< 3) ||| (trailControl <<< (3 + leadBitCount)) |]
       yield! [|
           for trailControl = 0 to (1 <<< trailBitCount) - 1 do
                      7 |||  (trailControl <<< (3)) |]
       yield 3
    |]
let  controlSymLen = [|
    yield! [|2;2|]
    yield! [| for leadControl = 0 to (1 <<< leadBitCount) - 1 do
                  for trailControl = 0 to (1 <<< trailBitCount) - 1 do
                      3 + leadBitCount + trailBitCount|]
    yield! [|
           for trailControl = 0 to (1 <<< trailBitCount) - 1 do
                      3 + trailBitCount|]
    yield 3
|]

let controlHuffman = Huffman.Huffman(controlSym,controlSymLen)
let  lut = StatusTable(trailBitCount)


let syms = [|
    for eIndex = 0 to elfControlSym.Length - 1 do
        for index = 0 to controlSym.Length - 1 do
            let startBit = elfControlSymLen[eIndex]
            elfControlSym[eIndex] ||| (controlSym[index] <<< startBit)
|]
let symsLen = [|
    for eIndex = 0 to elfControlSym.Length - 1 do
        for index = 0 to controlSym.Length - 1 do
            elfControlSymLen[eIndex] + (controlSymLen[index])
|]
let huffman = Huffman(syms,symsLen)