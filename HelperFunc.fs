module PaperCompress.HelperFunc

open System
open System.Diagnostics
open System.IO
open System.Runtime.InteropServices

let writeArrayToFile (filePath: string) (data: double array) =
    use writer = new BinaryWriter(File.Open(filePath, FileMode.Create))
    data |> Array.iter writer.Write

let bytesToArraySpan (bytes: byte[]) : 'T[] =
    if bytes.Length % 8 <> 0 then
        failwithf "字节数组长度必须是8的倍数，当前长度：%d" bytes.Length

    let span = ReadOnlySpan<byte>(bytes)
    let Span = MemoryMarshal.Cast<byte, 'T>(span)
    Span.ToArray()

let compressTest
    (compress: double array -> byte array)
    (decompress: byte array -> int -> double array)
    (compressData: double array)
    =
    let oriSize = compressData.Length * 8
    
    let stopwatch = Stopwatch.StartNew()
    let compressedData = compress compressData
    let data = decompress compressedData compressData.Length
    stopwatch.Stop()
    for i = 0 to data.Length - 1 do
        if data[i] <> compressData[i] then
            raise (Exception($"压缩算法存在问题,在解压第{i}个数据时出现错误"))
    Console.WriteLine($"共压缩{compressData.Length}个数据点")
    Console.WriteLine($"初始数据大小为{oriSize},压缩后大小为{compressedData.Length}")
    Console.WriteLine($"压缩率为{(double) (compressedData.Length) / (double) oriSize}")
    printfn "执行时间: %d ms" stopwatch.ElapsedMilliseconds
    printfn "精确时间: %O" stopwatch.Elapsed
    Console.WriteLine()

let getFoodPriceData =
    let DefaultDbFile = "/home/user/DataGripProjects/DataSet/dataset.db"

    []
