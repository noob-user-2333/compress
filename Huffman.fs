module Compress.Huffman

open System
open System.Collections.Generic
open System.Numerics
let private MaxSymIndex = int UInt16.MaxValue
type private Symbol =
    struct
        val symbol:int
        val len:int
        new (sym:int,len:int) = {symbol = sym;len = len}
    end
type private Node =
    struct
        val symIndex:int
        val score:int
        new (index:int,score:int) = {symIndex = index;score = score}
        interface IComparer<Node> with
            member this.Compare (x: Node, y: Node): int =
                let xValue = (((int64)x.score) <<< 16) + ((int64)x.symIndex)
                let yValue = (((int64)y.score) <<< 16) + ((int64)y.symIndex)
                sign(xValue-yValue)
    end
type private Tree =
    | Empty  // 叶子节点/空节点：无值、无子树
    | Tree of Value: Node * Left: Tree * Right: Tree


type Huffman(symbols:int array,symLen:int array) =
    let symbols = [|
        //保留0号索引用于表示哈夫曼非叶子节点的索引
        let count = symbols.Length
        if count > MaxSymIndex then
            raise (ArgumentException("暂不支持大于Uint16.MaxValue数量的符号种类计算哈夫曼树"))
        for i  = 0 to count do
            if i = 0 then
                Symbol(0,0)
            else    
                Symbol(symbols[i - 1],symLen[i - 1])
        |]
    let mutable tree = Empty
    static let extractNode (tree: Tree) : Node =
        match tree with
        // 匹配含Node的Tree Case，提取命名字段Value（即Node），返回Some node
        | Tree (Value = node)  -> node
        // 匹配Empty Case，无Node可提取，返回None
        | Empty -> raise(ArgumentException("必须确保用于提取的节点不为空"))
       
    static let update(freqArray:int array) =
        let queue = new PriorityQueue<Tree,Node>()
        for i = 1 to freqArray.Length do
            let freq = freqArray[i - 1]
            let node = Node(i,freq)
            let tree = Tree(node,Empty,Empty)
            queue.Enqueue(tree,node)
        while queue.Count > 1 do
            let tree1 = queue.Dequeue()
            let tree2 = queue.Dequeue()
            let node = Node(0,(extractNode tree1).score + (extractNode tree2).score)
            let tree = Tree(node,tree1,tree2)
            queue.Enqueue(tree,node)
        queue.Dequeue()
    member this.Update(freqArray:int array) =
        tree <- update freqArray