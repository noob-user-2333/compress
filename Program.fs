// For more information see https://aka.ms/fsharp-console-apps
open System
open System.Diagnostics
open System.IO
open System.Linq
open System.Runtime.InteropServices
open Compress
open Compress.HelperFunc
open Compress.Huffman
open Compress.ZeroCountModel
open Compress.tANS
open Compress.toolClass

let defaultDataFile = "/dev/shm/data"
// let dats = Dataset.getFoodPrices
// writeArrayToFile defaultDataFile dats
let bData = File.ReadAllBytes defaultDataFile
let binaryData = bData[1024 * 1024 .. 1024 * 1024 * 2 - 1]
let doubleData = bytesToArraySpan<double> binaryData
let data = doubleData[0..]
// HelperFunc.compressTest AFC.compress AFC.decompress data
// HelperFunc.compressTest Chimp.compress Chimp.decompress data
// HelperFunc.compressTest Gorilla.compress Gorilla.decompress data
let codeArray = [|for i = 0 to 255 do i|]
let lenArray = [|for i = 0 to 255 do 8|]
let freqArray = Array.zeroCreate<int>(256)
for d in binaryData do
    freqArray[int d] <- freqArray[int d] + 1
let useBit = 4
let model = ZeroCountModel(useBit,6)
let stopwatch = Stopwatch()
stopwatch.Start()
for i = 0 to 1024 do
    model.Update freqArray[0..63]
stopwatch.Stop()
Console.WriteLine($"耗时{stopwatch.ElapsedMilliseconds}ms")
// let normal = binaryData.Length * 8
// let mutable cost = 0
// for d in binaryData do
//     let control = model.GetControl(int d)
//     let num = model.GetNumFromControl control
//     let value = int d - num
//     cost <- cost + useBit + value
//     if value < 0 then
//         raise (Exception("error"))
// Console.WriteLine($"正常消耗{normal}bit，压缩后消耗{cost}bit")
// let rate =  double cost / double normal
// Console.WriteLine($"压缩率为{rate}")