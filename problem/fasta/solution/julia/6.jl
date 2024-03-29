# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# Contributed by Kristoffer Carlsson
# Updated by Adam Beckmeyer

import .Threads: @spawn

const LINESIZE = 60
const CHUNKSIZE = 1024LINESIZE

const IM = 139968 % Int32
const IA = 3877 % Int32
const IC = 29573 % Int32
const SEED = Ref(42 % Int32)

gen_random() = (SEED[] = ((SEED[] * IA) + IC) % IM)

# Vector{UInt8} faster indexing than Base.CodeUnits{UInt8,String} (julia 1.2)
const alu = Vector{UInt8}(string(
   "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG",
   "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA",
   "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT",
   "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA",
   "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG",
   "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC",
   "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"))

const iub1 = Vector{UInt8}("acgtBDHKMNRSVWY")
# Instead of the probability, iub2 is the integer upper-bounding the random
# integer to the equivalent index in iub1
const iub2 =
    floor.(Int32, IM .* cumsum([0.27, 0.12, 0.12, 0.27, 0.02, 0.02, 0.02, 0.02,
                                0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02]))
const homosapiens1 = Vector{UInt8}("acgt")
const homosapiens2 =
    floor.(Int32, IM .* cumsum([0.3029549426680, 0.1979883004921,
                                0.1975473066391, 0.3015094502008]))

# Defining a function with successive comparisons is faster than getindex lookup
for (op, symb, pr) in ((:iub, iub1, iub2),
                       (:homosapiens, homosapiens1, homosapiens2))
    # successively compares x with all values in pr and returns value from symb
    @eval function $op(x)
        $((:(x <= $p && return $s) for (p, s) in zip(pr, symb))...)
        'a' % UInt8
    end
end

function repeat_fasta(io, src, n)
    len = length(src)
    pos = 1
    for i=1:LINESIZE:n
        # l is the number of characters we need to write this iteration
        l = min(LINESIZE, n - i + 1)
        # m is the number of characters available to write from src
        m = min(l, len - pos + 1)
        GC.@preserve src unsafe_write(io, pointer(src, pos), m)

        if l != m # reached end of src
            # Loop back around to the start of src and write remaining chars
            pos = 1
            m = l - m
            GC.@preserve src unsafe_write(io, pointer(src, pos), m)
        end
        pos += m

        write(io, '\n')
    end
end

# Produce chunks of random numbers based on chunk-sizes taken from
# c1. Then put tasks into c2 that yield a chunk of random characters
# when complete.
function random_chunks(f, c1, c2, t)
    wait(t)
    for n in c1
        v = Vector{Int32}(undef, n)
        for i=1:n
            @inbounds v[i] = gen_random()
        end
        # putting a task into c2 instead of a vector allows multiple
        # of these map! calcs to be occurring in parallel.
        put!(c2, @spawn @inbounds map!(f, Vector{UInt8}(undef, n), v))
    end
    close(c2)
end

# Writes chunks of random characters from c to io with newlines added
# every LINESIZE.
function print_chunks(io, firstline, c, t)
    wait(t)
    write(io, firstline)
    for vt in c
        v = fetch(vt)::Vector{UInt8}
        n = length(v)
        for i=1:LINESIZE:n
            GC.@preserve v unsafe_write(io, pointer(v, i),
                                        min(LINESIZE, n - i + 1))
            write(io, '\n')
        end
    end
end

function random_fasta(io, flookup, n, firstline, rtask, iotask)
    chunksize = min(n, CHUNKSIZE)

    # c1 holds Ints indicating chunk size
    c1 = Channel{Int}(typemax(Int))
    # c2 holds tasks that yield vectors ready to be written to io
    c2 = Channel{Task}(16)

    t1 = @spawn random_chunks(flookup, c1, c2, rtask)
    # Making the IO thread sticky improves timing consistency
    t3 = @async print_chunks(io, firstline, c2, iotask)

    for p=1:chunksize:n
        put!(c1, min(chunksize, n - p + 1))
    end
    # This will cascade through to close c2 and end t1 and t3 once all
    # work is finished.
    close(c1)
    # Both t1 and t3 returned because we have to wait for all random
    # number generation to be finished before we can start generating
    # random numbers again, and we have to wait for all io to be
    # finished before we can start writing to io again.
    t1, t3
end

function fasta(n=25000000, io=stdout)
    write(io, ">ONE Homo sapiens alu\n")
    t1 = @spawn nothing
    t3 = @spawn repeat_fasta(io, alu, 2n)

    t1, t3 = random_fasta(io, iub, 3n, ">TWO IUB ambiguity codes\n", t1, t3)
    t1, t3 = random_fasta(io, homosapiens, 5n, ">THREE Homo sapiens frequency\n"
,
                          t1, t3)
    wait(t3)
end

isinteractive() || fasta(parse(Int,ARGS[1]))

