﻿(* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   - contributed by Vassil Keremidchiev, Otto Bommer's Scala program
   - modified by Peter Kese
*)

open System.Threading

let F = // factorials up to 20
    let mutable last = int64 1
    let next = function | 0 -> int64 1 | i -> last <- last * (int64 i); last
    Array.init 21 next

let fannkuch n task chunk =
    let p = Array.init n id
    let pp = Array.create n 0
    let count = Array.create n 0
    let mutable flips = 0
    let mutable cksum = 0

    let rec direct idx i =
        if i > 0 then
            let d = int (idx / F.[i])
            count.[i] <- d
            for j = 0 to d-1 do pp.[j] <- p.[j]
            for j = 0 to i-d do p.[j] <- p.[j+d]
            for j = 0 to d-1 do p.[j+i+1-d] <- pp.[j]
            direct (idx % F.[i]) (i-1)

    let inline permute () =
        let mutable first = p.[1]
        p.[1] <- p.[0]
        p.[0] <- first

        let mutable i = 1
        count.[i] <- count.[i] + 1
        while count.[i] > i do
            count.[i] <- 0
            i <- i + 1
            let next = p.[1]
            p.[0] <- next
            for j = 1 to i-1 do p.[j] <- p.[j+1]
            p.[i] <- first
            first <- next
            count.[i] <- count.[i] + 1

    let inline fcount () =
        let mutable flips = 1
        let mutable first = p.[0]

        if p.[first] <> 0 then
            for i = 0 to n-1 do pp.[i] <- p.[i]

            while pp.[first] <> 0 do
                flips <- flips + 1

                let mutable lo = 1
                let mutable hi = first - 1
                while lo < hi do
                    let t = pp.[lo]
                    pp.[lo] <- pp.[hi]
                    pp.[hi] <- t

                    lo <- lo + 1
                    hi <- hi - 1
                let t = pp.[first]
                pp.[first] <- first
                first <- t
        flips

    let lo = int64(task) * chunk
    let hi = min F.[n] (lo+chunk)

    direct lo (p.Length - 1)

    let last = int(hi - lo - 1L)
    for j = 0 to last do
        if p.[0] <> 0 then
            let f = fcount()
            flips <- max flips f
            cksum <- cksum + if (int64(j)+lo) % 2L = 0L then f else -f
        if j < last then permute()

    (cksum, flips)


let nthreads = System.Environment.ProcessorCount
let n = try int((System.Environment.GetCommandLineArgs()).[1]) with _ -> 7
let split (i:int64) = (F.[n] + i - 1L) / i
let chunk  = split (int64(nthreads * n))
let ntasks = int(split chunk)

let (c, fl) =
    [0..ntasks]
        |> Seq.map (fun i -> async { return fannkuch n i chunk } )
        |> Async.Parallel |> Async.RunSynchronously
        |> Array.fold (fun (_cksum, _flips) (cksum, flips) -> (_cksum + cksum, m
ax _flips flips)) (0,0)

printfn "%d\nPfannkuchen(%d) = %d" c n fl

