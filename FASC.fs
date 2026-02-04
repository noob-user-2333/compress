module Compress.FASC

open Compress.Algorithm
open Compress.toolClass

//目标1:从固定的两个算法中选取最优算法作为压缩结果
//目标2:使这两个算法能够共用相同预测器减少压缩消耗
//目标3:根据评估结果选取最优的两个算法作为压缩结果
//目标4:根据评估结果选取最优的4个算法作为压缩结果
//
//当前阶段:开始完成目标1

type Algorithm(index:int,algorithmInterface:IAlgorithm) =
    
    let algorithm = algorithmInterface
    











let compress (values: double[]) : uint64[] = [||]

let decompress (data: uint64[]) (count: int) : double[] = [||]
