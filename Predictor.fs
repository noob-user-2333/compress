module Compress.Predictor

open System
open toolClass

type PredictResult() =
    
    let mutable leadingZero = 0 
    let mutable trailZero = 0 
    let mutable payloadCount = 0 
    let mutable payload = 0UL
    let mutable pred = 0UL
    member this.Set(leading:int,trail:int,_payload:uint64,predNum:uint64) =
        leadingZero <- leading
        trailZero <- trail
        payloadCount <- 64 - leading - trail
        payload <- _payload
        pred <- predNum
    member this.LeadingZero = leadingZero
    member this.TrailZero = trailZero
    member this.PayloadCount = payloadCount
    member this.Payload = payload
    member this.Pred = pred
type IPredictor =
    abstract member Update:uint64 -> unit
    //传入当前数值，返回预测残差
    abstract member Predict: unit -> PredictResult
    abstract member Clear: unit -> unit
    
type LastValue() =
    let mutable last = 0UL
    let result = PredictResult()
    let leadingFreq = [| for i = 0 to 64 do 0 |]
    let trailFreq = [|for i = 0 to 64 do 0|]
    member this.LeadingFreq = leadingFreq
    member this.TrailFreq = trailFreq
    interface IPredictor with
        member this.Update(currentValue):unit =
            let xor = last ^^^ currentValue
            let leading = BitUtil.lz xor
            let trail = BitUtil.tz xor
            leadingFreq[leading] <- leadingFreq[leading] + 1
            trailFreq[trail] <- trailFreq[trail] + 1
            result.Set(leading,trail,xor,last)
            last <- currentValue
            
        member this.Predict() =
            result
        member this.Clear() =
            Array.Clear(leadingFreq)
            Array.Clear(trailFreq)
            
type PDFCM() =
    let predictTable = Array.zeroCreate 256
    let mutable prev = 0.0
    let mutable prevNum = 0UL
    let mutable hash = 0
    [<Literal>]
    let lowWordMask = 0xFFFFFFFFFFFFFUL
    [<Literal>]
    let highWordMask = 0xFFF0000000000000UL
    let leadingFreq = [| for i = 0 to 64 do 0 |]
    let trailFreq = [|for i = 0 to 64 do 0|]
    let result = PredictResult()
    let updateHash (deltaOfUint64: uint64) =
        let deltaNum = deltaOfUint64
        let deltaHighWord = deltaNum >>> 40
        hash <- ((hash <<< 2) ^^^ (int) (deltaHighWord &&& 0xFFUL)) &&& 0xFF

    let update  (currentValue: uint64) =
        let value = BitUtil.u2d currentValue
        let actualDelta = value - prev
        predictTable[hash] <- actualDelta
        updateHash (currentValue)
        prev <- value
        prevNum <- currentValue

    let predict() =
        let highWord = prevNum &&& highWordMask
        let predictDelta = predictTable[hash]
        let predict = prev + predictDelta
        let predictNum = BitUtil.d2u predict
        let result = highWord + (predictNum &&& lowWordMask)
        result
    member this.LeadingFreq = leadingFreq
    member this.TrailFreq = trailFreq
    interface IPredictor with
        member this.Update(currentValue) =
            update(currentValue)
            let value = predict()
            let xor = value ^^^ currentValue
            let leading = BitUtil.lz xor
            let trail = BitUtil.tz xor
            leadingFreq[leading] <- leadingFreq[leading] + 1
            trailFreq[trail] <- trailFreq[trail] + 1
            result.Set(leading,trail,xor,value)
        member this.Predict() =
            result    
        member this.Clear() =
            Array.Clear(leadingFreq)
            Array.Clear(trailFreq)
// 哈希表行结构：论文指定存储最近两次64位差分（dpred' / dpred''），值类型保证效率
[<Struct>]
type HashTableEntry = {
    mutable DPredPrime: int64  // 最近一次差分 dpred'（64位有符号，用于数值计算）
    mutable DPredDoublePrime: int64 // 第二次最近差分 dpred''
}

/// 三阶DFCM预测器（论文核心）：参考LastValue类风格实现IPredictor接口
type DFCM() =
    // 哈希表配置：论文指定2^20行（1,048,576），初始化为0
    let hashTableSize = 1 <<< 20
    let hashTable = Array.create hashTableSize { DPredPrime = 0L; DPredDoublePrime = 0L }
    // 三阶差分上下文：存储最近3个64位差分（delta1/delta2/delta3），初始为0（论文指定三阶）
    let mutable delta1 = 0L
    let mutable delta2 = 0L
    let mutable delta3 = 0L
    // 上一个浮点值（位模式，64位无符号）：用于计算差分/生成预测值，与LastValue的last语义一致
    let mutable last = 0UL
    // 预测结果实例：与LastValue的result字段完全一致
    let result = PredictResult()
    // 频率统计数组：与LastValue的leadingFreq/trailFreq完全一致（统计XOR的前导/尾随零分布）
    let leadingFreq = [| for i = 0 to 64 do 0 |]
    let trailFreq = [| for i = 0 to 64 do 0 |]
    // 哈希函数掩码：20位低位掩码（论文指定截取XOR结果的20位低位作为哈希索引）
    let lsb20Mask = 0x00000000000FFFFFL

    // 私有方法：论文指定的哈希函数（输入三阶差分的14位MSB，输出20位哈希索引）
    let computeHash (d1: uint64, d2: uint64, d3: uint64) =
        let d1Int = int64 d1
        let d2Int = int64 d2
        let d3Int = int64 d3
        // 论文公式：hash = lsb0-19(d1 XOR (d2<<5) XOR (d3<<10))
        let s2 = d2Int <<< 5
        let s3 = d3Int <<< 10
        let xorResult = d1Int ^^^ s2 ^^^ s3
        int (xorResult &&& lsb20Mask) // 截取20位低位作为哈希索引

    // 私有方法：判断两个差分是否相似（论文指定：前14位MSB相同）
    let isSimilar (d1: int64, d2: int64) =
        let d1UInt = uint64 d1
        let d2UInt = uint64 d2
        BitUtil.extractMSB14 d1UInt = BitUtil.extractMSB14 d2UInt

    // 私有方法：核心预测逻辑——根据三阶差分上下文预测下一个差分
    let predictDelta (dt1: int64, dt2: int64, dt3: int64, hashTable: HashTableEntry[]) =
        // 步骤1：提取三个差分的14位MSB特征（论文指定）
        let d1 = BitUtil.extractMSB14 (uint64 dt1)
        let d2 = BitUtil.extractMSB14 (uint64 dt2)
        let d3 = BitUtil.extractMSB14 (uint64 dt3)
        // 步骤2：计算哈希索引，查找哈希表
        let hashIdx = computeHash (d1, d2, d3)
        let entry = hashTable[hashIdx]
        // 步骤3：自适应预测（论文核心）：相似则线性外推，否则用最近差分
        if isSimilar (entry.DPredPrime, entry.DPredDoublePrime) then
            entry.DPredPrime + (entry.DPredPrime - entry.DPredDoublePrime) // 线性外推
        else
            entry.DPredPrime // 直接使用最近差分

    // 私有方法：更新哈希表（论文指定：dpred''=原dpred'，dpred'=真实差分）
    let updateHashTable (dt1: int64, dt2: int64, dt3: int64, realDelta: int64, hashTable: HashTableEntry[]) =
        let d1 = BitUtil.extractMSB14 (uint64 dt1)
        let d2 = BitUtil.extractMSB14 (uint64 dt2)
        let d3 = BitUtil.extractMSB14 (uint64 dt3)
        let hashIdx = computeHash (d1, d2, d3)
        let mutable entry = hashTable[hashIdx]
        // 先进先出更新：保留最近两次差分
        entry.DPredDoublePrime <- entry.DPredPrime
        entry.DPredPrime <- realDelta
        hashTable[hashIdx] <- entry
    // 公共只读属性：暴露频率统计数组，与LastValue类一致
    member this.LeadingFreq = leadingFreq
    member this.TrailFreq = trailFreq
    // 实现IPredictor接口：与LastValue类的接口实现完全一致
    interface IPredictor with
        /// 更新预测器：输入当前浮点值（64位位模式），更新差分/哈希表/频率统计
        member this.Update(currentValue: uint64) =
            // 步骤1：计算当前真实差分（论文指定：整数减法，浮点值转64位有符号后相减）
            let realDelta = int64 currentValue - int64 last
            // 步骤2：根据三阶差分上下文预测下一个差分
            let predDelta = predictDelta (delta1, delta2, delta3, hashTable)
            // 步骤3：生成预测值（前一个值 + 预测差分，转回64位无符号位模式）
            let predValueInt = int64 last + predDelta
            let predValue = uint64 predValueInt
            let xor = predValue ^^^ currentValue
            let leading = BitUtil.lz xor
            let trail = BitUtil.tz xor
            // 步骤5：更新频率统计数组（与LastValue类完全一致）
            leadingFreq[leading] <- leadingFreq[leading] + 1
            trailFreq[trail] <- trailFreq[trail] + 1
            // 步骤6：设置预测结果（与LastValue类的result.Set一致，新增预测值）
            result.Set(leading, trail, xor,predValue)
            last <- currentValue
            // 步骤7：更新哈希表（用真实差分）
            updateHashTable (delta1, delta2, delta3, realDelta, hashTable)
            // 步骤8：滑动窗口更新三阶差分上下文（d1<-d2, d2<-d3, d3<-realDelta）
            delta1 <- delta2
            delta2 <- delta3
            delta3 <- realDelta

        /// 预测：返回封装好的预测结果，与LastValue类完全一致
        member this.Predict() =
            result

        /// 清空预测器：重置所有状态，与LastValue类风格一致
        member this.Clear() =
            // 清空频率统计数组（与LastValue类完全一致）
            Array.Clear(leadingFreq)
            Array.Clear(trailFreq)
            