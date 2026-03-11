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
let testUnreadBits () =
    // 构造测试数据：两个 uint64
    // word0: 0xDEADBEEF_12345678
    // word1: 0xCAFEBABE_AABBCCDD
    let data = [| 0xDEADBEEF12345678UL; 0xCAFEBABEAABBCCDDUL; 0UL |]

    let r = BitReader(data)
    let mutable passed = 0
    let mutable failed = 0

    let check (name: string) (expected: uint64) (actual: uint64) =
        if expected = actual then
            passed <- passed + 1
            printfn $"  PASS: {name}"
        else
            failed <- failed + 1
            printfn $"  FAIL: {name} — expected 0x{expected:X}, got 0x{actual:X}"

    // ── 测试1：读 8 位，回退 8 位，再读 8 位，结果应该一样
    printfn "Test 1: Read + Unread + Re-read"
    let v1 = r.ReadBits(8)
    r.UnreadBits(8)
    let v2 = r.ReadBits(8)
    check "read-unread-reread 8bit" v1 v2

    // ── 测试2：读 20 位，回退 12 位，相当于只前进了 8 位
    printfn "Test 2: Partial unread"
    r.UnreadBits(8) // 回到起点
    let full20 = r.ReadBits(20)
    r.UnreadBits(12)
    // 现在位于 bit 8，读 12 位应等于 full20 的高 12 位
    let high12 = r.ReadBits(12)
    let expected12 = uint64 ((full20 >>> 8) &&& lowBitMask[12])
    check "partial unread 12 of 20" expected12 high12

    // ── 测试3：跨 word 边界回退
    printfn "Test 3: Cross-word boundary unread"
    r.UnreadBits(20) // 回到起点
    let _ = r.ReadBits(60) // 读到 word0 的 bit60
    let cross = r.ReadBits(8) // 跨越 word0/word1 边界，读 bit60..67
    r.UnreadBits(8)
    let cross2 = r.ReadBits(8)
    check "cross-word unread" cross cross2

    // ── 测试4：逐位读 vs 批量读一致性
    printfn "Test 4: Bit-by-bit vs batch after unread"
    r.UnreadBits(68) // 回到起点
    let batch = r.ReadBits(16)
    r.UnreadBits(16)

    let mutable bitByBit = 0UL
    for i = 0 to 15 do
        bitByBit <- bitByBit ||| (uint64 (r.ReadBit()) <<< i)

    check "bit-by-bit vs batch 16bit" batch bitByBit

    // ── 测试5：回退到 word 对齐位置（accPos=0）
    printfn "Test 5: Unread to word-aligned position"
    r.UnreadBits(16) // 回到起点
    let _ = r.ReadBits(64) // 恰好读完 word0
    r.UnreadBits(64) // 回退整个 word
    let word0 = r.ReadBits(64)
    check "unread full word" data[0] word0

    // ── 测试6：UnreadBits(0) 不应改变状态
    printfn "Test 6: Unread 0 bits (no-op)"
    r.UnreadBits(64) // 回到起点
    let before = r.ReadBits(32)
    r.UnreadBits(32)
    r.UnreadBits(0) // 应该是 no-op
    let after = r.ReadBits(32)
    check "unread 0 is no-op" before after

    // ── 汇总
    printfn ""
    printfn $"Results: {passed} passed, {failed} failed"

    if failed > 0 then
        failwith "UnreadBits tests failed!"
    else
        printfn "All UnreadBits tests passed!"

testUnreadBits()
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
