// For more information see https://aka.ms/fsharp-console-apps
open System
open System.Diagnostics
open System.IO
open System.Linq
open System.Numerics
open System.Runtime.CompilerServices
open System.Runtime.Intrinsics
open System.Runtime.Intrinsics.Arm
open System.Runtime.Intrinsics.X86
open Compress
open Compress.HelperFunc
open Compress.tANS
open Compress.toolClass

let defaultDataFile = "/dev/shm/data"
// let dats = Dataset.getFoodPrices
// writeArrayToFile defaultDataFile dats
let binaryData = File.ReadAllBytes defaultDataFile
let doubleData = bytesToArraySpan<double> binaryData
// let data = doubleData[0..]
// //
// HelperFunc.compressTest AFC.compress AFC.decompress data
// HelperFunc.compressTest Chimp.compress Chimp.decompress data
// HelperFunc.compressTest Gorilla.compress Gorilla.decompress data
//测试ANS
let freqArray = Array.zeroCreate<int>(256)
let count =   1024 * 1024 * 2
for d in binaryData[0 .. count / 256 - 1] do
    let index = int d
    freqArray[index] <- freqArray[index] + 1
let sum = freqArray.Sum()




let w = BitWriter()

let encodeANS = tANS(freqArray,8)
let watch = Stopwatch.StartNew()
for d in binaryData[0 .. count] do
    let data = int d
    encodeANS.Encode(data,w)
watch.Stop()
Console.WriteLine($"耗时{watch.ElapsedMilliseconds}ms")
let data = w.ToArray()
let r = BitReader(data)

let dData = Array.zeroCreate<int>(count)
let watch1 = Stopwatch.StartNew()

for i = 0 to 128 do
    let decodeANS = tANS(freqArray,8)
    1
// for i = 1 to count do
    // dData[i - 1] <- decodeANS.Decode(r)

watch1.Stop()
Console.WriteLine($"耗时{watch1.ElapsedMilliseconds}ms")
for i = 1 to count do
      if dData[count - 1 - (i - 1)] <>int binaryData[i - 1] then
          raise (Exception($"ANS解码存在问题，第{i - 1}号数据与实际不符"))