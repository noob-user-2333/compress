module Compress.Huffman





type Huffman(symbols:int array,symLen:int array) =
    let symbols = symbols
    let symLen = symLen
    
    
    let update(freqArray:int array) =
        1


    member this.Update(freqArray:int array) =
        update freqArray