// For more information see https://aka.ms/fsharp-console-apps
open System
open System.Diagnostics
open System.IO
open System.Linq
open System.Runtime.InteropServices
open Compress
open Compress.HelperFunc
open Compress.Huffman
open Compress.Predictor
open Compress.ZeroCountModel
open Compress.tANS
open Compress.toolClass
open Compress.Elf
let defaultDataFile = "/dev/shm/data"
let dats = Dataset.getCityTemp
writeArrayToFile defaultDataFile dats
let bData = File.ReadAllBytes defaultDataFile
let binaryData = bData[0 .. 1024 * 1024 * 2 - 1]
let doubleData = bytesToArraySpan<double> binaryData
let data = doubleData[0..]
// HelperFunc.compressTest AFC.compress AFC.decompress data
HelperFunc.compressTest Elf.compress Elf.decompress data
HelperFunc.compressTest Gorilla.compress Gorilla.decompress data
HelperFunc.compressTest Chimp.compress Chimp.decompress data
// let codeArray = [|for i = 0 to 255 do i|]
// let lenArray = [|for i = 0 to 255 do 8|]
// let freqArray = Array.zeroCreate<int>(256)
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
