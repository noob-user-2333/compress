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
let binaryData = File.ReadAllBytes defaultDataFile
let doubleData = bytesToArraySpan<double> binaryData
// let data = doubleData[0..]
// //
// HelperFunc.compressTest AFC.compress AFC.decompress data
// HelperFunc.compressTest Chimp.compress Chimp.decompress data
// HelperFunc.compressTest Gorilla.compress Gorilla.decompress data
open Compress.Huffman
open System

// 测试用例1：基础多符号测试（核心验证编码解码正确性+压缩效果）
let testBasicHuffmanEncodeDecode () =
        // ========== 1. 初始化测试参数 ==========
        let testSyms = [|10; 20; 30; 40; 50|] // 测试符号集（自定义任意整数符号）
        let testSymLens = [|6; 6; 6; 6; 6|]   // 符号原始长度（单位：比特，与符号一一对应）
        let freqArray = [| 100; 50; 20; 10; 5|] // 频率数组：索引0丢弃，1-5对应testSyms的5个符号（频率越高，编码越短）
        let originalSymbols = [|10;10;10;20;20;30;40;50;10;20;30;10|] // 待编码的原始符号序列

        // ========== 2. 构建哈夫曼实例并初始化 ==========
        let huffman = Huffman(testSyms, testSymLens)
        // 给哈夫曼类的freqArray赋值（必须步骤：频率是构建哈夫曼树的依据）
        huffman.Update(freqArray) // 构建哈夫曼树

        // ========== 3. 哈夫曼编码：将原始符号序列编码为比特流 ==========
        let bitWriter = BitWriter()
        let mutable encodeTotalBits = 0 // 编码后总比特数
        for sym in originalSymbols do
            let mutable bitCount = 0
            let encodeVal = huffman.Encode(sym, &bitCount) // 编码单个符号，返回编码值+输出比特数
            bitWriter.WriteBits(uint64 encodeVal, bitCount) // 将编码值写入BitWriter
            encodeTotalBits <- encodeTotalBits + bitCount

        // ========== 4. 从比特流中读取数据，准备解码 ==========
        let (totalBitCount, encodeData) = bitWriter.ToArray() // 获取编码后的比特流数据+总比特数
        let bitReader = BitReader(encodeData) // 构建BitReader，用于解码

        // ========== 5. 哈夫曼解码：从比特流还原符号序列 ==========
        let mutable decodeSymbols = ResizeArray<int>() // 解码后的符号序列
        let mutable decodeTotalBits = 0
        while decodeSymbols.Count < originalSymbols.Length do
            let mutable bitCount = 0
            let decodeSym = huffman.Decode(bitReader, &bitCount) // 解码单个符号
            decodeSymbols.Add(decodeSym)
            decodeTotalBits <- decodeTotalBits + bitCount

        // ========== 6. 验证结果：原始序列 vs 解码序列 ==========
        let isEqual = originalSymbols = decodeSymbols.ToArray()
        // 统计原始数据总比特数（用于对比压缩效果）
        let originalTotalBits = originalSymbols.Length * testSymLens[0]
        // 输出测试结果
        printfn "===== 基础多符号测试结果 ====="
        printfn "原始符号序列：%A" originalSymbols
        printfn "解码符号序列：%A" (decodeSymbols.ToArray())
        printfn "编码解码是否一致：%b" isEqual
        printfn "原始总比特数：%d bit" originalTotalBits
        printfn "编码后总比特数：%d bit" encodeTotalBits
        printfn "压缩率：%.2f%%" (100.0 - (float encodeTotalBits / float originalTotalBits) * 100.0)
        printfn "================================\n"

// 测试用例2：边界测试-单符号重复序列（验证单符号场景下编码解码正确性）
let testSingleSymbolHuffman () =
        // 单符号测试：仅符号88，频率100，重复编码10次
        let testSyms = [|88|]
        let testSymLens = [|8|]
        let freqArray = [| 100|]
        let originalSymbols = [|88;88;88;88;88;88;88;88;88;88|]

        let huffman = Huffman(testSyms, testSymLens)
        huffman.Update(freqArray)

        // 编码
        let bitWriter = BitWriter()
        let mutable encodeTotalBits = 0
        for sym in originalSymbols do
            let mutable bitCount = 0
            let encodeVal = huffman.Encode(sym, &bitCount)
            bitWriter.WriteBits(uint64 encodeVal, bitCount)
            encodeTotalBits <- encodeTotalBits + bitCount

        // 解码
        let (_, encodeData) = bitWriter.ToArray()
        let bitReader = BitReader(encodeData)
        let mutable decodeSymbols = ResizeArray<int>()
        while decodeSymbols.Count < originalSymbols.Length do
            let mutable bitCount = 0
            let decodeSym = huffman.Decode(bitReader, &bitCount)
            decodeSymbols.Add(decodeSym)

        let isEqual = originalSymbols = decodeSymbols.ToArray()
        let originalTotalBits = originalSymbols.Length * testSymLens[0]
        printfn "===== 单符号重复测试结果 ====="
        printfn "原始符号序列：%A" originalSymbols
        printfn "解码符号序列：%A" (decodeSymbols.ToArray())
        printfn "编码解码是否一致：%b" isEqual
        printfn "原始总比特数：%d bit | 编码后总比特数：%d bit" originalTotalBits encodeTotalBits
        printfn "================================\n"
        isEqual

// 测试用例3：边界测试-空序列（验证鲁棒性，无报错）
let testEmptySymbolSequence () =
    try
        let testSyms = [|1;2;3|]
        let testSymLens = [|4;4;4|]
        let freqArray = [| 10; 20; 30|]
        let originalSymbols = [||] // 空序列

        let huffman = Huffman(testSyms, testSymLens)
        huffman.Update(freqArray)

        // 编码空序列
        let bitWriter = BitWriter()
        for sym in originalSymbols do
            let mutable bitCount = 0
            let encodeVal = huffman.Encode(sym, &bitCount)
            bitWriter.WriteBits(uint64 encodeVal, bitCount)

        // 解码空序列
        let (_, encodeData) = bitWriter.ToArray()
        let bitReader = BitReader(encodeData)
        let mutable decodeSymbols = ResizeArray<int>()
        while decodeSymbols.Count < originalSymbols.Length do
            let mutable bitCount = 0
            let decodeSym = huffman.Decode(bitReader, &bitCount)
            decodeSymbols.Add(decodeSym)

        let isEqual = originalSymbols = decodeSymbols.ToArray()
        printfn "===== 空序列测试结果 ====="
        printfn "空序列编码解码是否无报错且一致：%b" isEqual
        printfn "================================\n"
        isEqual
    with
    | ex ->
        printfn "空序列测试失败：%s" ex.Message
        false

// 统一执行所有测试用例
let runAllHuffmanTests () =
    printfn "开始执行哈夫曼编码所有测试用例...\n"
    let test1 = testBasicHuffmanEncodeDecode ()
    let test2 = testSingleSymbolHuffman ()
    let test3 = testEmptySymbolSequence ()
    printfn "========================================="
    printfn "所有测试用例执行完成："
    printfn "========================================="

// 启动测试（直接调用此方法即可运行所有测试）
runAllHuffmanTests ()