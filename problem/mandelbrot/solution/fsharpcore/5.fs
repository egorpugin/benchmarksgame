// The Computer Language Benchmarks Game
// https://benchmarksgame-team.pages.debian.net/benchmarksgame/
//
// Adapted by Antti Lankila from the earlier Isaac Gouyʼs implementation
// Add multithread & tweaks from C++ by The Anh Tran
// Translate to F# by Jomo Fisher
// ported from C# version by Anthony Lloyd
module Mandelbrot

#nowarn "9"

open System
open System.Numerics
open System.Runtime.CompilerServices
open System.Threading.Tasks
open Microsoft.FSharp.NativeInterop

[<EntryPoint>]
let main args =
    let inline padd p i = p+8n*i
    let inline ptrGet p i = Unsafe.Read((p+8n*i).ToPointer())
    let inline ptrSet p i v = Unsafe.Write((p+8n*i).ToPointer(), v)
    let inline getByte (ciby:float) pcrbi =
        let rec calc i res =
            if i=8n then res
            else
                let vCrbx = ptrGet pcrbi i
                let vCiby = Vector ciby
                let mutable zr = vCrbx
                let mutable zi = vCiby
                let mutable j = 49
                let mutable b = 0
                while b<>3 && j>0 do
                    j <- j-1
                    let nZr = zr * zr - zi * zi + vCrbx
                    zi <- let zrzi = zr * zi in zrzi + zrzi + vCiby
                    zr <- nZr
                    let t = nZr * nZr + zi * zi
                    b <- b ||| if t.[0]>4.0 then 2 else 0
                           ||| if t.[1]>4.0 then 1 else 0
                calc (i+2n) ((res <<< 2) + b)
        calc 0n 0 ^^^ -1 |> byte
    let size = if args.Length=0 then 200 else int args.[0]
    let lineLength = size >>> 3
    let s = "P4\n"+string size+" "+string size+"\n"
    let data = Array.zeroCreate (size*lineLength+s.Length)
    Text.ASCIIEncoding.ASCII.GetBytes(s, 0, s.Length, data, 0) |> ignore
    let crb = Array.zeroCreate (size+2)
    use pdata = fixed &data.[s.Length]
    use pcrb = fixed &crb.[0]
    let pcrbi = NativePtr.toNativeInt pcrb
    let invN = Vector (2.0/float size)
    let onePtFive = Vector 1.5
    let step = Vector 2.0
    let rec loop i value =
        if i<size then
            ptrSet pcrbi (nativeint i) (value*invN-onePtFive)
            loop (i+2) (value+step)
    Vector [|0.0;1.0;0.0;0.0;0.0;0.0;0.0;0.0|] |> loop 0
    Parallel.For(0, size, fun y ->
        let ciby = NativePtr.get pcrb y+0.5
        for x = 0 to lineLength-1 do
            nativeint x*8n |> padd pcrbi |> getByte ciby
            |> NativePtr.set pdata (y*lineLength+x)
    ) |> ignore
    Console.OpenStandardOutput().Write(data, 0, data.Length)
    0

