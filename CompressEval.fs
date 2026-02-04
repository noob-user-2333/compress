module Compress.CompressEval

open System
open Compress.toolClass

[<CustomEquality;CustomComparison>]
type CompressResult =
    struct
        val bitCount: byte
        val bitControl:  byte
        val bitResult: byte
        val unused: byte
        val control: int
        val result : uint64
        new( bitControl, bitResult, control, result) =
            {
                bitCount   = bitControl + bitResult
                bitResult  = bitResult
                bitControl = bitControl
                unused     = 0uy
                control    = control
                result     = result
            }
        member inline x.Key : int = ((int x.bitCount) <<< 16) ||| ((int x.bitControl) <<< 8) ||| (int x.bitResult)
        override x.Equals(o: obj) =
            match o with
            | :? CompressResult as y -> x.Key = y.Key
            | _ -> false
        override x.GetHashCode() =
            x.Key   
        interface IComparable<CompressResult> with
            member x.CompareTo(y: CompressResult) =
                compare x.Key y.Key
        interface IComparable with
            member x.CompareTo(o: obj) =
                match o with
                | :? CompressResult as y -> compare x.Key y.Key
                | _ -> invalidArg "o" "not a CompressResult"              
    end
type  IAlgorithm =
    abstract Update : unit -> unit
    abstract Compress :  data:double[] -> index:int -> CompressResult
    abstract Decompress : r:BitReader -> double
 