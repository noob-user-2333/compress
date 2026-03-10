module Compress.LUT

[<Struct>]
type LUTEntry =
    val private packed: byte

    new(mode: int, useLead: int, useTrail: int, useLast: int) =
        { packed = byte (mode ||| (useLead <<< 3) ||| (useTrail <<< 4) ||| (useLast <<< 5)) }
    member this.Alg = int this.packed &&& 1
    member this.Mode = int this.packed &&& 0x07
    member this.UseLead = (int this.packed >>> 3) &&& 1
    member this.UseTrail = (int this.packed >>> 4) &&& 1
    member this.UseLast = (int this.packed >>> 5) &&& 1

type StatusTable(trailBit: int) =
    let stateTbl = Array.zeroCreate<byte> 129
    let lut = Array.zeroCreate<LUTEntry> 64
    let mutable curTrailBit = trailBit

    let rebuild (tb: int) =
        for u = 0 to 128 do
            let v = u - 64

            let sdl =
                if v < 0 then 0uy
                elif v = 0 then 1uy
                else 2uy

            let sdt =
                if v < 0 then 0uy
                elif v <= tb then 1uy
                else 2uy

            let st =
                if v < 0 then 0uy
                elif v = 64 then 0uy
                elif v < tb then 1uy
                else 2uy

            stateTbl[u] <- sdl ||| (sdt <<< 2) ||| (st <<< 4)

        let m101 = LUTEntry(5, 1, 1, 0)
        let m00 = LUTEntry(0, 0, 0, 1)
        let m01 = LUTEntry(2, 0, 0, 1)
        let m110 = LUTEntry(3, 0, 0, 0)
        let m111 = LUTEntry(7, 0, 1, 0)

        for idx = 0 to 63 do
            let sdl = idx &&& 0x03
            let sdt = (idx >>> 2) &&& 0x03
            let st = (idx >>> 4) &&& 0x03

            lut[idx] <-
                match sdl, sdt, st with
                | 0, _, _ -> m101
                | 2, 0, _ -> m101
                | 2, _, 0 -> m00
                | 2, _, _ -> m01
                | 1, _, 0 -> m00
                | 1, _, 1 -> m110
                | 1, 0, 2 -> m111
                | 1, 2, 2 -> m111
                | 1, 1, 2 -> m01
                | _ -> m101

        curTrailBit <- tb

    do rebuild trailBit

    member _.Rebuild(newTrailBit: int) =
        if newTrailBit <> curTrailBit then
            rebuild newTrailBit

    member _.TrailBit = curTrailBit

    member _.Lookup(deltaL: int, deltaT: int, t: int) =
        let sdl = stateTbl[deltaL + 64] &&& 0x03uy
        let sdt = (stateTbl[deltaT + 64] >>> 2) &&& 0x03uy
        let st = (stateTbl[t + 64] >>> 4) &&& 0x03uy
        let idx = int sdl ||| (int sdt <<< 2) ||| (int st <<< 4)
        lut[idx]

    static member inline Select(cur: int, prev: int, useLast: int) =
        (cur &&& (useLast - 1)) ||| (prev &&& (-useLast))
