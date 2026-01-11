module Compress.betterCompress

open System.Collections.Generic
open toolClass


let compress (values: double[]) : byte[] =
    let w = BitWriter()

    //使用字典标识高位
    let dict = new Dictionary<uint64, int>()
    let highWordDictMap = Array.zeroCreate (values.Length)
    let deltaMap = Array.zeroCreate (values.Length)
    let leadCount = Array.zeroCreate (values.Length)
    let leadDelta = Array.zeroCreate (values.Length)
    let leadDict = new Dictionary<int, int>()
    let mutable lastDictId = 0
    let mutable prev = 0UL
    let mutable prevLead = 0

    for i = 0 to values.Length - 1 do
        let value = BitUtil.d2u values[i]
        let highWord = value >>> 52
        let lowWord = value &&& 0xFFFFFFFFFFFFFUL
        //---------------
        //符号位与指数位处理
        //---------------
        if dict.ContainsKey(highWord) = false then
            dict.Add(highWord, dict.Count)

        let xorv = prev ^^^ value
        let lead = BitUtil.lz xorv
        leadCount[i] <- lead
        leadDelta[i] <- lead - prevLead

        if leadDict.ContainsKey(lead) = false then
            leadDict.Add(lead, 0)

        leadDict[lead] <- leadDict[lead] + 1
        prev <- value
        prevLead <- lead
        highWordDictMap[i] <- dict[highWord]
        deltaMap[i] <- highWordDictMap[i] - lastDictId
        lastDictId <- highWordDictMap[i]
        //------------
        //开始处理尾数
        //------------
        1

    w.ToArray()

let decompress (bytes: byte[]) (length: int) : double[] = [||]
