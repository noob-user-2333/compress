// For more information see https://aka.ms/fsharp-console-apps
open System
open System.Diagnostics
open System.IO
open System.Linq
open Compress
open Compress.HelperFunc
open Compress.Huffman
open Compress.tANS
open Compress.toolClass

let defaultDataFile = "/dev/shm/data"
// let dats = Dataset.getFoodPrices
// writeArrayToFile defaultDataFile dats
let bData = File.ReadAllBytes defaultDataFile
let binaryData = bData[0 .. 1024 * 1024 * 8 - 1]
let doubleData = bytesToArraySpan<double> binaryData
// let data = doubleData[0..]
// //
// HelperFunc.compressTest AFC.compress AFC.decompress data
// HelperFunc.compressTest Chimp.compress Chimp.decompress data
// HelperFunc.compressTest Gorilla.compress Gorilla.decompress data
let codeArray = [|for i = 0 to 255 do i|]
let lenArray = [|for i = 0 to 255 do 8|]
let freqArray = Array.zeroCreate<int>(256)
for d in binaryData do
    freqArray[int d] <- freqArray[int d] + 1
let tree = Huffman.Huffman(codeArray,lenArray)
tree.Update freqArray
let len = Array.zeroCreate<int>(binaryData.Length)
let w = BitWriter()
let stopwatch = Stopwatch()
stopwatch.Start()
for i = 0 to binaryData.Length - 1 do
    let d = binaryData[i]
    let mutable j = 0
    let sym = tree.Encode(int d,&j)
    w.WriteBits(uint64 sym,j)
    len[i] <- j
stopwatch.Stop()
let huffmanCodeTime  = stopwatch.ElapsedMilliseconds
let w1 = BitWriter()
stopwatch.Reset()
stopwatch.Restart()
for i = 0 to binaryData.Length - 1 do
    let d = binaryData[i]
    w1.WriteBits(uint64 d,len[i])
stopwatch.Stop()
Console.WriteLine($"哈夫曼编码耗时{huffmanCodeTime}ms,正常编码耗时{stopwatch.ElapsedMilliseconds}ms")
let bitCount,data = w.ToArray()
let bits,decodeData1 = w1.ToArray()
let r = BitReader(data)
stopwatch.Reset()
stopwatch.Restart()
for i = 1 to binaryData.Length do
    let mutable j = 0
    let sym = (tree.Decode(r,&j))
    sym
stopwatch.Stop()
let decodeTime = stopwatch.ElapsedMilliseconds
stopwatch.Reset()
stopwatch.Restart()
let r1 = BitReader(decodeData1)
for i = 1 to binaryData.Length do
    let sym = r1.ReadBits(len[i - 1])
    sym
stopwatch.Stop()
Console.WriteLine($"哈夫曼解码耗时{decodeTime}ms,正常解码耗时{stopwatch.ElapsedMilliseconds}ms")
let rate = (double)bitCount / (double)(binaryData.Length * 8)
Console.WriteLine($"压缩率为{rate}")        