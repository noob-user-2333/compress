// For more information see https://aka.ms/fsharp-console-apps
open System
open System.Diagnostics
open System.IO
open Compress
open Compress.AFC
open Compress.Chimp
open Compress.Elf
open Compress.ElfUtil
open Compress.Gorilla
open Compress.HelperFunc
open Compress.IAAC
open Compress.toolClass

let defaultDataFile = "/dev/shm/data"
let dats = Dataset.getCityTemp
writeArrayToFile defaultDataFile dats
let bData = File.ReadAllBytes defaultDataFile
let binaryData = bData
let doubleData = bytesToArraySpan<double> binaryData
let data = doubleData
let iaac = IAAC()
let compressors:ICompressor array = [|Gorilla();Chimp();AFC();Elf()|]
for c in compressors do
    compressTest c data
zeroLeadModule.Update zeroLeadFreqArray[0..63]
zeroTrailModule.Update zeroTrailFreqArray[0..63]
compressTest iaac data

// HelperFunc.compressTest Elf.compress Elf.decompress data

// HelperFunc.compressTest Elf.compress Elf.decompress data
// HelperFunc.compressTest IAAC.compress IAAC.decompress data
// HelperFunc.compressTest IAAC.compress IAAC.decompress data
// HelperFunc.compressTest IAAC.compressNoHuff IAAC.decompressNoHuff data
// HelperFunc.compressTest IAAC.compressNoHuff IAAC.decompressNoHuff data
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
