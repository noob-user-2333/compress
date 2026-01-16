// For more information see https://aka.ms/fsharp-console-apps
open System.IO
open Compress
open Compress.HelperFunc

let defaultDataFile = "/dev/shm/data"
let dats = Dataset.getFoodPrices
writeArrayToFile defaultDataFile dats
let binaryData = File.ReadAllBytes defaultDataFile
let doubleData = bytesToArraySpan<double> binaryData
let data = doubleData[0..]
// let d = betterCompress.compress data
HelperFunc.compressTest AFC.compress AFC.decompress data
HelperFunc.compressTest Chimp.compress Chimp.decompress data
HelperFunc.compressTest Gorilla.compress Gorilla.decompress data
