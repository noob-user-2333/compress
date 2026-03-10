module Compress.Huffman
open System
open Compress.toolClass
open System.Collections.Generic

type private Symbol =
    struct
        val code: int
        val len: int
        new(code, len) = { code = code; len = len }
    end

type private Node =
    struct
        val symIndex: int
        val index: int
        val lIndex: int
        val rIndex: int

        new(index, symIndex, l, r) =
            { index = index
              symIndex = symIndex
              lIndex = l
              rIndex = r }
    end

type Huffman(syms: int array, symLen: int array) =

    let symCount = syms.Length
    let maxSym = Array.max syms

    // symbols[0] = dummy, symbols[1..symCount] = actual symbols
    let symbols =
        [| yield Symbol(0, 0)
           for i = 0 to symCount - 1 do
               yield Symbol(syms[i], symLen[i]) |]

    let symMap = Array.zeroCreate<Symbol>(maxSym + 1)
    let freqArray = Array.zeroCreate<int>(maxSym + 1)

    // 解码树：预分配数组，避免 ResizeArray 开销
    let mutable nodeCapacity = max (4 * symCount + 32) 256
    let mutable nodes = Array.zeroCreate<Node>(nodeCapacity)
    let mutable nodeCount = 0
    let mutable mergedSymIndices = Array.empty<int>
    let mutable mergedSuffixBits = 0
    let mutable rootNodeIndex = 0
    let mutable active = false

    let ensureNodeCapacity (needed: int) =
        if needed > nodeCapacity then
            let newCap = max (nodeCapacity * 2) needed
            let newArr = Array.zeroCreate<Node>(newCap)
            Array.Copy(nodes, newArr, nodeCount)
            nodes <- newArr
            nodeCapacity <- newCap

    // ═══════════════════════════════════════════
    // 用符号值本身作为码字构建解码树（与 initIdentity 编码一致）
    //
    // initIdentity: symMap[sym] = Symbol(sym, symLen)
    // 即 code = 符号值本身，len = 预设码长
    // WriteBits/ReadBit 都是 LSB first
    // 所以解码树按 sym 值的 bit0→bit(len-1) 顺序构建
    // ═══════════════════════════════════════════
    let buildIdentityTree () =
        ensureNodeCapacity(4 * symCount + 32)
        nodeCount <- 1
        // nodes[0] = 哨兵，lIndex/rIndex = 0 表示"无子节点"
        nodes[0] <- Node(0, 0, 0, 0)

        // 根节点 = index 1
        let rootIdx = nodeCount
        nodeCount <- nodeCount + 1
        nodes[rootIdx] <- Node(rootIdx, 0, 0, 0)

        // 填充 symMap（identity: code = 符号值）
        Array.Clear(symMap)

        for i = 0 to symCount - 1 do
            let code = syms[i]
            let l = symLen[i]
            symMap[code] <- Symbol(code, l)

            if l > 0 then
                let symIdx = i + 1 // 1-indexed into symbols[]
                let mutable cur = rootIdx

                // LSB first: bit 0 先读，对应树的第一层
                for bit = 0 to l - 1 do
                    let b = (code >>> bit) &&& 1
                    let nd = nodes[cur]

                    if bit = l - 1 then
                        // 最后一位 → 创建叶节点
                        let leafIdx = nodeCount
                        nodeCount <- nodeCount + 1
                        ensureNodeCapacity(nodeCount + 1)
                        nodes[leafIdx] <- Node(leafIdx, symIdx, 0, 0)

                        if b = 0 then
                            nodes[cur] <- Node(cur, nd.symIndex, leafIdx, nd.rIndex)
                        else
                            nodes[cur] <- Node(cur, nd.symIndex, nd.lIndex, leafIdx)
                    else
                        // 中间位 → 导航或创建内部节点
                        let child =
                            if b = 0 then nd.lIndex else nd.rIndex

                        if child = 0 then
                            let newIdx = nodeCount
                            nodeCount <- nodeCount + 1
                            ensureNodeCapacity(nodeCount + 1)
                            nodes[newIdx] <- Node(newIdx, 0, 0, 0)

                            if b = 0 then
                                nodes[cur] <- Node(cur, nd.symIndex, newIdx, nd.rIndex)
                            else
                                nodes[cur] <- Node(cur, nd.symIndex, nd.lIndex, newIdx)

                            cur <- newIdx
                        else
                            cur <- child

        mergedSymIndices <- Array.empty
        mergedSuffixBits <- 0
        rootNodeIndex <- rootIdx

    // ═══════════════════════════════════════════
    // 从频率数组构建自适应霍夫曼树（Update 使用）
    // ═══════════════════════════════════════════
    let build (fmin: int) =
        ensureNodeCapacity(2 * symCount + 16)
        nodeCount <- 1
        nodes[0] <- Node(0, 0, 0, 0)

        let queue = PriorityQueue<int * int, int>()
        let lowFreq = ResizeArray<int>()

        for i = 1 to symCount do
            let sym = symbols[i]
            let freq = freqArray[sym.code]

            if freq <= fmin then
                lowFreq.Add(i)
            else
                let idx = nodeCount
                nodeCount <- nodeCount + 1
                ensureNodeCapacity(nodeCount + 1)
                nodes[idx] <- Node(idx, i, 0, 0)
                queue.Enqueue((idx, freq), freq)

        if lowFreq.Count > 0 then
            let idx = nodeCount
            nodeCount <- nodeCount + 1
            ensureNodeCapacity(nodeCount + 1)
            nodes[idx] <- Node(idx, -1, 0, 0)
            queue.Enqueue((idx, fmin), fmin)

        mergedSymIndices <- lowFreq.ToArray()

        mergedSuffixBits <-
            if lowFreq.Count <= 1 then
                0
            else
                int (Math.Ceiling(Math.Log2(float lowFreq.Count)))

        while queue.Count > 1 do
            let i1, f1 = queue.Dequeue()
            let i2, f2 = queue.Dequeue()
            let idx = nodeCount
            nodeCount <- nodeCount + 1
            ensureNodeCapacity(nodeCount + 1)
            nodes[idx] <- Node(idx, 0, i1, i2)
            queue.Enqueue((idx, f1 + f2), f1 + f2)

        if nodeCount = 2 then
            let n = nodes[1]
            nodeCount <- nodeCount + 1
            ensureNodeCapacity(nodeCount + 1)
            nodes[2] <- Node(2, 0, n.index, 0)

        rootNodeIndex <- nodeCount - 1

    let rec assignCodes nodeIndex prefix level =
        let node = nodes[nodeIndex]

        match node.symIndex with
        | si when si > 0 -> symMap[symbols[si].code] <- Symbol(prefix, level)
        | -1 ->
            let totalLen = level + mergedSuffixBits

            for i = 0 to mergedSymIndices.Length - 1 do
                let sym = symbols[mergedSymIndices[i]]
                symMap[sym.code] <- Symbol(prefix ||| (i <<< level), totalLen)
        | _ -> ()

        if node.lIndex > 0 then
            assignCodes node.lIndex prefix (level + 1)

        if node.rIndex > 0 then
            assignCodes node.rIndex ((1 <<< level) + prefix) (level + 1)

    // ═══════════════════════════════════════════
    // 初始化：构建 identity 解码树
    // ═══════════════════════════════════════════
    do buildIdentityTree ()

    // 保存初始状态，用于 Reset 快速恢复
    let initSymMap = Array.copy symMap
    let initNodes = Array.zeroCreate<Node>(nodeCount)
    do Array.Copy(nodes, initNodes, nodeCount)
    let initNodeCount = nodeCount
    let initRootNodeIndex = rootNodeIndex

    // ═══════════════════════════════════════════
    // 公开接口
    // ═══════════════════════════════════════════

    member _.Active = active

    /// 根据累积频率重建最优霍夫曼树
    member _.Update(fmin: int) =
        build fmin
        Array.Clear symMap
        assignCodes rootNodeIndex 0 0
        Array.Clear freqArray
        active <- true

    member this.Update() = this.Update(0)

    /// 恢复到构造时的初始状态（重建 identity 树，而非清空）
    member _.Reset() =
        Array.Clear freqArray
        Array.Copy(initSymMap, symMap, initSymMap.Length)
        Array.Copy(initNodes, nodes, initNodeCount)
        nodeCount <- initNodeCount
        rootNodeIndex <- initRootNodeIndex
        mergedSymIndices <- Array.empty
        mergedSuffixBits <- 0
        active <- false

    /// 编码：符号 → struct(码字, 位数)
    member _.Encode(symbol: int) =
        freqArray[symbol] <- freqArray[symbol] + 1
        let s = symMap[symbol]
        struct (s.code, s.len)

    /// 解码：从 BitReader 逐位读取 → struct(符号, 位数)
    member _.Decode(r: BitReader) =
        let mutable ni = rootNodeIndex
        let mutable node = nodes[ni]

        while node.symIndex = 0 do
            let bit = r.ReadBit()
            ni <- if bit = 0 then node.lIndex else node.rIndex
            node <- nodes[ni]

        let sym =
            if node.symIndex = -1 then
                let mutable suffix = 0

                for b = 0 to mergedSuffixBits - 1 do
                    suffix <- suffix ||| (r.ReadBit() <<< b)

                symbols[mergedSymIndices[suffix]]
            else
                symbols[node.symIndex]

        freqArray[sym.code] <- freqArray[sym.code] + 1
        struct (sym.code, sym.len)

