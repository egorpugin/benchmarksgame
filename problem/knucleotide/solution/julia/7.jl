# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# Contributed by Adam Beckmeyer

import Printf: @printf

const LINESIZE = 61
const SIZEHINT = 4 * 4096
const FRAMES = (1, 2, 3, 4, 6, 12, 18)
const COUNTSFOR = codeunits.(("ggt", "ggta", "ggtatt", "ggtattttaatt", "ggtatttt
aatttatagt"))

struct Seq{T<:Unsigned}
    x::T
end
Seq(T) = Seq(0 % T)

# 10 works marginally better for this hashtable than previously used 7
Base.hash(s::Seq, h::UInt)::UInt = s.x ⊻ s.x >> 10

make_mask(frame) = (UInt64(1) << 2frame) - 1

# Gives a = 0x00, c = 0x01, g = 0x03, t = 0x02
encode(c) = (c >> 1) & 0x03

# Return `c` such that `c == decode(encode(c))`.
function decode(b::UInt8)
    for c in ('a' % UInt8, 'c' % UInt8, 'g' % UInt8)
        b == encode(c) && return c
    end
    't' % UInt8
end

# Push encoding for `c` to last two bits of `s`.
push(s::Seq{T}, c, mask) where T = Seq((mask & ((s.x << 2) | encode(c))) % T)

function to_string(s::Seq, n)
    out = Vector{UInt8}(undef, n)
    for i in eachindex(out)
        @inbounds out[i] = decode((s.x & 0x03) % UInt8)
        s = Seq(s.x >> 2)
    end
    String(reverse!(out))
end

function count_frame!(d::Dict{Seq{T}}, seq, frame) where T
    mask = make_mask(frame)
    v = Seq(zero(T))
    # Fill the Seq up with `frame` pairs of bits
    for i=1:(frame-1)
        v = @inbounds push(v, seq[i], mask)
    end
    # Slide Seq character-by-character over the remainder of sequence
    for i=frame:length(seq)
        v = @inbounds push(v, seq[i], mask)
        # This does two hashtable lookups currently. Doing better
        # isn't possible until julia gets something like
        # https://github.com/JuliaLang/julia/pull/33758
        d[v] = get(d, v, 0) + 1
    end
end

struct SeqFreq
    seq::String
    freq::Int32
    total::Int32
end

# A sequence of characters that occurs `freq` times out of `total`.
SeqFreq(seq::Seq, freq, frame, total) = SeqFreq(to_string(seq, frame), freq, tot
al)

# Defined so that arrays of SeqFreq can be sort!ed
Base.isless(a::SeqFreq, b::SeqFreq) =
    a.freq == b.freq ? isless(a.seq, b.seq) : isless(a.freq, b.freq)

function Base.show(io::IO, x::SeqFreq)
    write(io, uppercase(String(x.seq)), ' ')
    @printf(io, "%2.3f", 100x.freq / x.total)
end

function write_freqs(io, d, frame, total)
    seqfreqs = [SeqFreq(seq, d[seq], frame, total) for seq in keys(d)]
    sort!(seqfreqs; rev=true)
    for seqf in seqfreqs
        println(io, seqf)
    end
    write(io, '\n')
end

# Lifted directly from knucleotide-julia-6
function get_third_seq(io)
    count = 0
    buffer_size = SIZEHINT
    buffer = Vector{UInt8}(undef, buffer_size)
    empty!(buffer)
    linebuffer = Vector{UInt8}(undef, LINESIZE)
    while !eof(io)
        if count === 3
            resize!(linebuffer, LINESIZE)
            new_length = length(buffer) + LINESIZE
            if new_length > buffer_size
                buffer_size = nextpow(2, nextpow(2, new_length))
                sizehint!(buffer, buffer_size)
            end#if
            nb = readbytes!(io, linebuffer)
            resize!(linebuffer, nb - 1)
            append!(buffer, linebuffer)
        else
            pos = position(io)
            nb = readbytes!(io, linebuffer)
            @inbounds count += first(linebuffer) === '>' % UInt8
            if last(linebuffer) !== '\n' % UInt8
                @inbounds seek(io, pos + findnext(isnewline, linebuffer, 1))
            end#if
        end#if
    end#while
    buffer
end#function

isnewline(c::UInt8)::Bool = c === '\n' % UInt8

function main(io, out)
    seq = get_third_seq(io)

    freqs = ((Dict{Seq{UInt32},Int32}() for _=1:6)..., Dict{Seq{UInt64},Int32}()
)
    Threads.@threads for i=1:7
        @inbounds count_frame!(freqs[i], seq, FRAMES[i])
    end

    @inbounds write_freqs(out, freqs[1], FRAMES[1], length(seq) - 1)
    @inbounds write_freqs(out, freqs[2], FRAMES[2], length(seq) - 2)

    mask = make_mask(18)
    for (i, v) in enumerate(COUNTSFOR)
        d = @inbounds freqs[i+2]
        frame = @inbounds FRAMES[i+2]
        # This is ugly but less ugly than having an entirely separate
        # loop for a single UInt64 Dict
        k = frame == 18 ? Seq(UInt64) : Seq(UInt32)
        for c in v
            k = push(k, c, mask)
        end
        write(out, string(get(d, k, 0)), '\t', uppercase(String(v)), '\n')
    end

    freqs
end

isinteractive() || main(stdin, stdout)

