module Compress.ZeroCountModel

open System


//功能:传入0数量出现的频次和记载这些0数量使用的bit，
//    计算出能使记录0消耗数量最小的每个数字对应数字关系


type ZeroCountModel(bitCount: int) =
    let k = 1 <<< bitCount
    let numControlMap = Array.zeroCreate<int> 65
    let controlNumMap = Array.zeroCreate<int> (k + 1)

    let P = Array.zeroCreate<int> 65
    let Q = Array.zeroCreate<int> 65

    do
        let interval = 64 / k

        for i = 0 to k - 1 do
            controlNumMap[i] <- i * interval

        controlNumMap[k] <- 64

        for i = 0 to 63 do
            numControlMap[i] <- min (i / interval) (k - 1)

        numControlMap[64] <- k

    member _.BitCount = bitCount
    member _.K = k

    member internal _.NumControlMap = numControlMap
    member internal _.ControlNumMap = controlNumMap

    member this.UpdateArray (srcNCM: int[]) (srcCNM: int[]) =
        Array.Copy(srcNCM, numControlMap, min srcNCM.Length numControlMap.Length)
        Array.Copy(srcCNM, controlNumMap, min srcCNM.Length controlNumMap.Length)
        numControlMap[64] <- k
        controlNumMap[k] <- 64

    member this.Copy() =
        let c = ZeroCountModel(bitCount)
        c.UpdateArray numControlMap controlNumMap
        c

    member this.Update(freqArray: ReadOnlySpan<int>) =
        if freqArray.Length <> 64 then
            raise (System.ArgumentException $"数组长度必须为64，当前为{freqArray.Length}")

        P[0] <- 0
        Q[0] <- 0

        for i = 0 to 63 do
            P[i + 1] <- P[i] + freqArray[i]
            Q[i + 1] <- Q[i] + freqArray[i] * i

        if P[64] = 0 then
            ()
        else

            let idxList = ResizeArray<int>()
            idxList.Add(0)

            for j = 1 to 63 do
                if freqArray[j] > 0 then
                    idxList.Add(j)

            idxList.Add(64)
            let idx = idxList.ToArray()
            let Z = idx.Length - 1

            let useK = min k Z

            let costM = Array2D.zeroCreate<int> (Z + 1) (Z + 1)

            for i = 0 to Z do
                for j = i to Z do
                    costM[i, j] <- (Q[idx[j]] - Q[idx[i]]) - idx[i] * (P[idx[j]] - P[idx[i]])

            let dp = Array2D.create (useK + 1) (Z + 1) System.Int32.MaxValue
            let pre = Array2D.create (useK + 1) (Z + 1) -1

            dp[1, 0] <- costM[0, Z]
            pre[1, 0] <- -1

            if useK >= 2 then
                for j = 1 to Z - 1 do
                    dp[2, j] <- costM[0, j] + costM[j, Z]
                    pre[2, j] <- 0

            for kk = 3 to useK do
                for j = kk - 1 to Z - 1 do
                    for i = kk - 2 to j - 1 do
                        if dp[kk - 1, i] < System.Int32.MaxValue then
                            let v = dp[kk - 1, i] - costM[i, Z] + costM[i, j] + costM[j, Z]

                            if v < dp[kk, j] then
                                dp[kk, j] <- v
                                pre[kk, j] <- i

            let mutable bestJ = useK - 1
            let mutable bestCApp = System.Int32.MaxValue

            for j = useK - 1 to Z - 1 do
                if dp[useK, j] < bestCApp then
                    bestCApp <- dp[useK, j]
                    bestJ <- j

            let approxPruned = Array.zeroCreate<int> useK
            let mutable j = bestJ

            for kk = useK downto 1 do
                approxPruned[kk - 1] <- j
                j <- pre[kk, j]

            let approxValues = Array.map (fun i -> idx[i]) approxPruned

            for i = 0 to useK - 1 do
                controlNumMap[i] <- approxValues[i]

            let remaining = k - useK

            if remaining > 0 then
                let lastApprox = approxValues[useK - 1]
                let gap = 64 - lastApprox
                let interval = max 1 (gap / (remaining + 1))

                for i = 0 to remaining - 1 do
                    controlNumMap[useK + i] <- min (lastApprox + (i + 1) * interval) 63

            controlNumMap[k] <- 64

            for i = 0 to k - 2 do
                let start = controlNumMap[i]
                let stop = controlNumMap[i + 1]

                for j = start to stop - 1 do
                    numControlMap[j] <- i

            for j = controlNumMap[k - 1] to 63 do
                numControlMap[j] <- k - 1

            numControlMap[64] <- k

    member _.GetControl(num: int) = numControlMap[num]
    member _.GetNumFromControl(control: int) = controlNumMap[control]
