# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# Contributed by Adam Beckmeyer

using Printf

const COUNTSFOR = ("ggt", "ggta", "ggtatt", "ggtattttaatt", "ggtattttaatttatagt"
)

# This is not reading the file in line-by-line. Doing so is impossible
# in Julia without using FFI because all io is automatically
# buffered. Thus despite not appearing to be buffered reads, all are.
function getseq3(io)
    # First read all the file preceding the 3rd sequence.
    for _=1:3
        readuntil(io, ʼ>ʼ)
    end
    readline(io)
    # Then read the third sequence in.
    buf = read(io)

    # In place, remove all newlines from buf and encode nucleotides
    # using only the last 2 bits in each byte.
    i = 1
    for c in buf
        if c != 0x0a # ʼ\nʼ
            # Gives a -> 0x00, c -> 0x01, g -> 0x03, t -> 0x02
            @inbounds buf[i] = c >> 1 & 0x03
            i += 1
        end
    end
    resize!(buf, i - 1)
end

# Decoding a single encoded nucleotide results in a byte ʼaʼ, ʼcʼ, ʼgʼ or ʼtʼ
decode(c) = c == 0x00 ? 0x61 : c == 0x01 ? 0x63 : c == 0x03 ? 0x67 : 0x74

# Decoding a UInt32 or UInt64 results in a string. This function
# creates the string from the last n encoded nucleotides in c.
function decode(c, n)
    buf = Vector{UInt8}(undef, n)
    for i=1:n
        @inbounds buf[n+1-i] = decode(c % UInt8 & 0x03)
        c = c >> 2
    end
    String(buf)
end

# Count the number of each subsequence of length n present in seq and
# store the result in hashtable d.
function count_subseqs!(d::Dict{T}, seq, n) where T
    # This results in a number where the last 2n bits are 1 and the rest are 0.
    mask = (1 << 2n - 1) % T
    key = zero(T)

    # Fill key with the first n-1 encoded nucleotides
    for i=1:n-1
        key = key << 2 | @inbounds seq[i]
    end

    # Slide key over the remainder of the sequence. Because of mask,
    # only the last n nucleotides are encoded in key.
    for i=n:length(seq)
        key = mask & (key << 2 | @inbounds seq[i])
        inc!(d, key)
    end
end

# If k exists in dictionary, increment it by one. Otherwise set the
# value of k to 1. If julia ever gets an update!/modify! function for
# changing hashtable values with one lookup, this function can be
# removed.
@inline function inc!(d::Dict{T,Int32}, k::T) where T
    index = Base.ht_keyindex2!(d, k)

    if index > 0
        # Positive index means the key already exists in the dictionary
        @inbounds d.vals[index] += 1
    else
        # Negative index means the key doesnʼt yet exist
        @inbounds Base._setindex!(d, 1, k, -index)
    end
end

# Define a fast hashing function for UInt keys encoded with the
# sequence. Defining a Base function on a Base type is "type-piracy"
# and should be avoided in production code. Instead, Base.hash could
# be defined on a wrapper struct. For this workload, this simple
# xor/bitshift hash speeds things up by over 20% compared to Juliaʼs
# default hash.
Base.hash(x::Unsigned)::UInt = x ⊻ x >> 7

# Create a sorted array with the number of appearances of each
# sequence of length n.
function frequency_table(seq, n)
    d = Dict{UInt32,Int32}()
    count_subseqs!(d, seq, n)

    total = sum(values(d))
    counts = [decode(k, n) => get(d, k, 0) / total for k in keys(d)]
    sort!(counts; lt=freq_isless, rev=true)
end

# This function is used for sorting the frequency table generated by
# the function above. Primary order is by frequency which is second
# in pair. Secondary order is reverse order of string which is first
# in pair.
freq_isless(a, b) = a[2] == b[2] ? isless(b[1], a[1]) : isless(a[2], b[2])

# Count the number of times subseq appears in seq by creating a
# hashtable of all sequences of the same length as subseq appearing in
# seq.
function count_sequence(seq, subseq)
    d = length(subseq) < 16 ? Dict{UInt32,Int32}() : Dict{UInt,Int32}()
    count_subseqs!(d, seq, length(subseq))

    # Construct the key needed for accessing the count of subseq
    key = 0 % UInt
    for c in subseq
        key = key << 2 | (c >> 1 & 0x03)
    end

    get(d, key, 0)
end

function main(io, out)
    seq = getseq3(io)

    # First spawn threads for counting occurrences of subseqs in seq
    # since they take longer than the frequency tables.
    counts = Vector{Int32}(undef, 5)
    counts_task = Threads.@spawn @sync for i=1:5
        Threads.@spawn @inbounds counts[i] =
            count_sequence(seq, codeunits(COUNTSFOR[i]))
    end

    # Because of Juliaʼs threading overhead, itʼs faster to just
    # calculate both the 1 and 2 nucleotide frequency tables on the
    # main thread while counts_task continues in background.
    for (a, b) in frequency_table(seq, 1)
        @printf(out, "%s %2.3f\n", uppercase(a), 100b)
    end
    println(out)

    for (a, b) in frequency_table(seq, 2)
        @printf(out, "%s %2.3f\n", uppercase(a), 100b)
    end
    println(out)

    wait(counts_task)
    @inbounds for i=1:5
        println(out, counts[i], ʼ\tʼ, uppercase(COUNTSFOR[i]))
    end
end

isinteractive() || main(stdin, stdout)

