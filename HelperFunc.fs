module Compress.HelperFunc

open System
open System.Diagnostics
open System.IO
open System.Runtime
open System.Runtime.InteropServices
open FSharp.NativeInterop
open Compress.toolClass
open System.Runtime.InteropServices
open System.Runtime.InteropServices.Marshalling
open FSharp.NativeInterop

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
    (algorithm:ICompressor)
    (compressData: double array)
    =
    let w = BitWriter()
    let doubleCount = compressData.Length &&& (~~~7)
    let ptr = NativeMemory.AlignedAlloc(unativeint (doubleCount * sizeof<double>),unativeint 64)
    let oriSize = doubleCount * 8
    Marshal.Copy(compressData,0, NativePtr.toNativeInt(NativePtr.ofVoidPtr<byte> ptr),doubleCount)
    let ptrBuffer = PtrBuffer(ptr,doubleCount)
    let reserverMemory = 256*1024 * 1024
    //预热
    // ── 预热：让 JIT 编译完所有方法 ──
    for _ = 1 to 3 do
        let w1 = BitWriter()
        let struct(bitCount1,testCompressedData1) = algorithm.Compress (w1,ptrBuffer)
        let r1 = BitReader(testCompressedData1)
        let _ = algorithm.Decompress (r1,doubleCount)
        ignore |> ignore
    //统计压缩耗时
    GCSettings.LargeObjectHeapCompactionMode <- GCLargeObjectHeapCompactionMode.CompactOnce
    GC.Collect(GC.MaxGeneration, GCCollectionMode.Aggressive, true, true)
    GC.WaitForPendingFinalizers()
    GC.Collect(GC.MaxGeneration, GCCollectionMode.Aggressive, true, true)
    // 2. 预留 16GB，关闭 GC
    if not (GC.TryStartNoGCRegion(reserverMemory)) then
        printfn "NoGCRegion 启动失败，内存不足"
    let stopwatch = Stopwatch.StartNew()
    let struct(bitCount,compressedData) = algorithm.Compress (w,ptrBuffer)
    stopwatch.Stop()
    GC.EndNoGCRegion()
    let compressTimeMs = stopwatch.ElapsedMilliseconds
    let compressedSize = bitCount / 8

    //正式开始解压
    let r = BitReader(compressedData)
    GCSettings.LargeObjectHeapCompactionMode <- GCLargeObjectHeapCompactionMode.CompactOnce
    GC.Collect(GC.MaxGeneration, GCCollectionMode.Aggressive, true, true)
    GC.WaitForPendingFinalizers()
    GC.Collect(GC.MaxGeneration, GCCollectionMode.Aggressive, true, true)
    // 2. 预留 16GB，关闭 GC
    if not (GC.TryStartNoGCRegion(reserverMemory)) then
        printfn "NoGCRegion 启动失败，内存不足"
    let stopwatch1 = Stopwatch.StartNew()
    let data = algorithm.Decompress (r,doubleCount)
    stopwatch1.Stop()
    GC.EndNoGCRegion()
    let decompressTimeMs = stopwatch1.ElapsedMilliseconds
    
    for i = 0 to data.Length - 1 do
        if data[i] <> compressData[i] then
            raise (Exception($"压缩算法存在问题,在解压第{i}个数据时出现错误"))
    
    NativeMemory.AlignedFree(ptr)
    Console.WriteLine($"算法:{algorithm.GetName()}")
    Console.WriteLine($"共压缩{compressData.Length}个数据点")
    Console.WriteLine($"初始数据大小为{oriSize},压缩后大小为{compressedSize}")
    Console.WriteLine($"压缩率为{(double) (compressedSize) / (double) oriSize}")
    printfn "压缩耗时: %d ms     解压耗时: %d ms" compressTimeMs decompressTimeMs
    Console.WriteLine()

let getFoodPriceData =
    let DefaultDbFile = "/home/user/DataGripProjects/DataSet/dataset.db"

    []
