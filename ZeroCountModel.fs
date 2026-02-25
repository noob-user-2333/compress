module Compress.ZeroCountModel

open System

type private CostEval =
    struct
        val cost:int
        val lastPos:int
        new (cost,last) = {cost = cost;lastPos = last}
    end    
//功能:传入0数量出现的频次和记载这些0数量使用的bit，
//    计算出能使记录0消耗数量最小的每个数字对应数字关系

type ZeroCountModel(bitCount:int,fullBit:int) =
    let bitCount = bitCount
    let fullNum = (1 <<< fullBit)
    let maxNum = (1 <<< bitCount) - 1
    //将传入的0数量转化为bitCount位数的控制位
    let numControlMap =
        let interval = fullNum / (maxNum + 1)
        let mutable num = -1
        [|
            for i = 0 to fullNum do
                if i % interval = 0 then
                    num <- num + 1
                num    
        |]
    //将控制位转化为对应的0数量
    let controlNumMap =
        let interval = fullNum / (maxNum + 1)
        [|
            for i = 0 to maxNum do
                i * interval
        |]
    let dp = Array.zeroCreate<CostEval>((maxNum + 1) * fullNum)
    let costArray = Array.zeroCreate<int>(fullNum)
    let idx(line)(col) =  line*fullNum + col
    let cost = Array.zeroCreate<int>(fullNum * fullNum)
    member this.BitCount = bitCount    
    member this.Update(freqArray:int array) =
        if freqArray.Length <> fullNum then
            raise (ArgumentException($"请确保数组长度为{fullNum}"))
        let costArray = costArray
        let cost = cost
        //首先计算成本数组    
        for i = 1 to fullNum - 1 do
            costArray[i] <- costArray[i - 1] + freqArray[i] * i
            dp[idx 0 i] <- CostEval(costArray[i],0)
        //预先计算costFunc
        for start = 0 to fullNum - 1 do
            for finish = start to fullNum - 1 do
                cost[idx start finish] <- costArray[finish] - costArray[start] - (finish - start) * start
        //开始动态规划
        //control代表代表值或控制位，第0个控制位必须是0
        for control = 1 to maxNum do
            //t代表在表示0~t dp[control][t]代表第control个值代表0~t的最小消耗
            //dp[y][x] = min(dp[y-1][t] + cost(t,x))
            for t = 1 to fullNum - 1 do
                let mutable minCost = Int32.MaxValue
                let mutable lastPos = 0
                for i = 1 to t do
                    let costbit = dp[idx (control - 1) i].cost + cost[idx i t]
                    if minCost > costbit then
                        minCost <- costbit
                        lastPos <- i
                dp[idx control t] <- CostEval(minCost,lastPos)
        let mutable last = dp[idx maxNum fullNum - 1].lastPos
        controlNumMap[maxNum] <- last
        for control = maxNum - 1 downto 1 do
            last <- dp[idx control last].lastPos
            controlNumMap[control] <- last
        last <- fullNum
        for i = maxNum - 1 downto 1 do
            let start = controlNumMap[i]
            for j = start to last - 1 do
                numControlMap[j] <- i
            last <- start
    member this.GetControl(num:int) =
        numControlMap[num]
    member this.GetNumFromControl(control:int) =
        controlNumMap[control]