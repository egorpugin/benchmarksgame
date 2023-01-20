// The Computer Language Benchmarks Game
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
//
// Modification by Don Syme & Jomo Fisher to use a nullable Next element
// Based on F# version by Robert Pickering
// Based on ocaml version by Troestler Christophe & Isaac Gouy
// Multithreaded by Anthony Lloyd, Peter Kese

type Next = Next of Tree * Tree
and [<Struct>] Tree = Tree of Next

[<EntryPoint>]
let main args =
    let minDepth = 4
    let maxDepth =if args.Length=0 then 10 else max (minDepth+2) (int args.[0])
    let stretchDepth = maxDepth + 1

    let rec make depth =
        if depth=0 then Tree Unchecked.defaultof<_>
        else Next (make (depth-1), make (depth-1)) |> Tree

    let check t =
        let rec tailCheck (Tree n) acc =
            if isNull (n :> obj) then acc
            else
                let (Next(l, r)) = n
                tailCheck l (tailCheck r acc+2)
        tailCheck t 1

    let stretchTreeCheck = System.Threading.Tasks.Task.Run(fun () ->
        let check = make stretchDepth |> check |> string
        "stretch tree of depth "+string stretchDepth+"\t check: "+check )

    let longLivedTree =
        let tree = make maxDepth
        tree, System.Threading.Tasks.Task.Run(fun () ->
            let check = check tree |> string
            "long lived tree of depth "+string maxDepth+"\t check: "+check
        )

    let nSizes = (maxDepth-minDepth)/2+1
    let treesAndDepths = Array.zeroCreate nSizes
    let checks = Array.zeroCreate nSizes
    let jobs = seq { // split tree rendering into jobs for parallel processing
        for slot = nSizes-1 downto 0 do
            let depth = minDepth+slot*2
            let nTrees = 1 <<< (maxDepth - depth + minDepth)
            treesAndDepths.[slot] <- nTrees,depth
            let nJobs = // split last tree into multiple jobs to optimize cpu
                if slot = 0 then 16 else 1
            let jobSize = nTrees / nJobs
            for _ in 1..nJobs do
                yield
                    fun () ->
                        let mutable c = 0
                        for _ in 1 .. jobSize do c <- c + (make depth |> check)
                        slot, c
    }
    System.Threading.Tasks.Parallel.ForEach(jobs, fun job ->
        let slot,c = job()
        lock checks (fun () -> checks.[slot] <- checks.[slot] + c)
    ) |> ignore

    stretchTreeCheck.Result |> printfn "%s"
    (treesAndDepths, checks)
    ||> Seq.iter2 (fun (nTrees,depth) checks ->
        printfn "%d\t trees of depth %d\t check: %d" nTrees depth checks)
    (snd longLivedTree).Result |> printfn "%s"
    0

