module Compress.Algorithm


type IAlgorithm =
    abstract member init: int -> unit
    abstract member update: int -> int
    abstract member predict: unit -> int
