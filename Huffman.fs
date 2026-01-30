module Compress.Huffman

open System
open System.Collections.Generic
open System.Linq
open System.Numerics
open System.Runtime.InteropServices
open Compress.toolClass

let private MaxSymIndex = int UInt16.MaxValue
type private Symbol =
    struct
        val symbol:int
        val len:int
        new (sym:int,len:int) = {symbol = sym;len = len}
    end
type private Node =
    struct
        val score:int
        val symIndex:int
        val index:int
        val lIndex:int
        val rIndex:int
        new (index:int,symIndex:int,score:int,lIndex:int,rIndex:int) = {index=index;symIndex = symIndex;score = score;lIndex = lIndex;rIndex = rIndex}

    end


type Huffman(syms:int array,symLen:int array) =
    let symbols = [|
        //保留0号索引用于表示哈夫曼非叶子节点的索引
        let count = syms.Length
        if count > MaxSymIndex then
            raise (ArgumentException("暂不支持大于Uint16.MaxValue数量的符号种类计算哈夫曼树"))
        for i  = 0 to count do
            if i = 0 then
                Symbol(0,0)
            else    
                Symbol(syms[i - 1],symLen[i - 1])
        |]
    let nodeArray = ResizeArray<Node>()
    //传入符号，映射为symbols的对应索引值
    let symIndexMap = Array.zeroCreate<int>(syms.Max() + 1)
    //传入符号，映射为哈夫曼编码
    let symTreeMap = Array.zeroCreate<Symbol>(syms.Max() + 1)
    
    do
        if syms.Length = 0 || syms.Length <> symLen.Length then
            raise (ArgumentException("请确保传入数组不为空且符号数组和符号长度数组的长度一致"))
        for i = 1 to syms.Length - 1 do
            let sym = symbols[i]
            let len = symLen[i]
            symIndexMap[sym.symbol] <- i
            
    let mutable rootNodeIndex = 0
    let mutable useTree = true

    static let update(nodeArray:ResizeArray<Node>,freqArray:int array) =
        nodeArray.Clear()
        //0号元素默认丢弃
        let queue = new PriorityQueue<Node,Node>()
        let node = Node(0,0,0,0,0)
        nodeArray.Add(node)
        for i = 1 to freqArray.Length do
            let freq = freqArray[i - 1]
            let node = Node(i,i,freq,0,0)
            nodeArray.Add(node)
            queue.Enqueue(node,node)
        while queue.Count > 1 do
            let node1 = queue.Dequeue()
            let node2 = queue.Dequeue()
            let node = Node(nodeArray.Count,0,node1.score + node2.score,node1.index,node2.index)
            nodeArray.Add(node)
            queue.Enqueue(node,node)
        //检测是否仅存在单一节点，如果是则进行单独处理
        if nodeArray.Count = 2 then
            let node = Node(2,0,nodeArray[1].score,nodeArray[1].index,0)
            nodeArray.Add(node)
        nodeArray.Count - 1    
    let rec genHuffmanCodeAndCost(freqArray:int array)(nodeIndex:int)(prefix:int)(level:int) =
        let mutable cost = 0
        let node = nodeArray[nodeIndex]
        if node.symIndex > 0 then
            let sym = symbols[node.symIndex]
            cost <- cost + (sym.len - level) * freqArray[node.symIndex - 1]
            symTreeMap[sym.symbol] <- Symbol(prefix,level)
        if node.lIndex > 0 then
            cost <- cost + genHuffmanCodeAndCost freqArray node.lIndex prefix (level + 1)
        if node.rIndex > 0 then
            cost <- cost + genHuffmanCodeAndCost freqArray node.rIndex ((1 <<< level) + prefix) (level + 1)
        cost    
    member this.Update(freqArray:int array) =
        rootNodeIndex <- update(nodeArray,freqArray)
        //遍历哈夫曼树确定每个符号编码
        //验证使用哈夫曼编码是否能够节省空间
        Array.Clear symTreeMap
        let nodeIndex = rootNodeIndex
        let cost = genHuffmanCodeAndCost freqArray nodeIndex 0 0
        if cost >= 0 then
            useTree <- true
        else
            useTree <- false
    member this.Encode(symbol:int,[<Out>] bitCount:byref<int>)=
        if useTree = false then
            bitCount <- 0
            symbol
        else
            let sym = symTreeMap[symbol]
            bitCount <- sym.len
            sym.symbol
    member this.Decode(r:BitReader,[<Out>] bitCount:byref<int>) =
        if useTree = false then
            let mutable num = r.ReadBit()
            let mutable count = 1
            while symIndexMap[num] = 0 do
                num <- (num <<< 1) + r.ReadBit()
                count <- count + 1
            bitCount <- count    
            num
        else
            let mutable nodeIndex = rootNodeIndex
            while nodeArray[nodeIndex].symIndex = 0 do
                let node = nodeArray[nodeIndex] 
                let bit = r.ReadBit()
                if bit = 0 then
                    nodeIndex <- node.lIndex
                else
                    nodeIndex <- node.rIndex
            let sym = symbols[nodeArray[nodeIndex].symIndex]        
            bitCount <- sym.len
            sym.symbol