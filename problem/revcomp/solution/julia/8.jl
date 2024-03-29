# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# Written by Jens Adam
# using code and ideas from
# -- David Campbell, Jarrett Revels, Kristoffer Carlsson,
# -- Alex Arslan, Olof Salberger, Adam Beckmeyer
# Optimized by Adam Beckmeyer

const LINE_LEN = 60

const INIO = stdin
const OUTIO = stdout

const COMPS = Vector{UInt8}(undef, 256)
for (i, j) in zip("AaCcGgTtUuMmRrWwSsYyKkVvHhDdBbNn",
                  "TTGGCCAAAAKKYYWWSSRRMMBBDDHHVVNN")
    @inbounds COMPS[i % UInt8] = j % UInt8
end

# Reverse the sequence chunk by chunk and print it.
# Looping "chunkwise" elides check for newline characters every loop.
function print_sequence(v)
    l = 1
    # Don't count the last newline
    r = length(v) - 1

    # The number of characters on the last line of the sequence determines the
    # left-side chunk-size
    chunkl = r % (LINE_LEN + 1)
    # The right-side chunk-size is everything left in each line
    chunkr = LINE_LEN - chunkl

    # Each loop iteration handles 1 full line from left of vector and
    # end-half + next start-half from right of vector.
    while true
        # Check whether next chunk for l and r is same chunk
        l + chunkl - 1 == r && break
        revcomp_chunks!(v, l, r, chunkl)
        l += chunkl
        # chunkl + 1 skips over a newline index
        r -= chunkl + 1

        l + chunkr - 1 == r && break
        revcomp_chunks!(v, l, r, chunkr)
        l += chunkr + 1
        r -= chunkr
    end

    # revcomp the last chunk between l and r
    revcomp_chunks!(v, l, r, cld(r - l + 1, 2))

    write(OUTIO, v)
end

# revcomp the n elements in v starting with l with the n elements ending with r
function revcomp_chunks!(v, l, r, n)
    for i=0:n-1
        li, ri = l + i, r - i
        @inbounds v[li], v[ri] = COMPS[v[ri]], COMPS[v[li]]
    end
end

function perf_revcomp()
    seek(INIO, 1)
    while !eof(INIO)
        # Seek back 1 since previous loop put position on '>'
        seek(INIO, position(INIO) - 1)
        # Write header line
        write(OUTIO, readline(INIO; keep=true))
        # Perform revcomp on sequence.
        # Julia buffers all IOStreams by no more than 131072
        # bytes. While readuntil does not fulfill the letter of the
        # law in "read line-by-line", doing so is impossible since
        # regardless of user machinations, Julia always reads 128 kb
        # at a time from io; there is no way to disable this
        # buffering. Other implementations (e.g. Matt Brubeck's Rust
        # 2) also use this pattern. The only difference is that
        # Julia's buffer-size is 128 kb while Rust's is only 8 kb.
        print_sequence(readuntil(INIO, '>' % UInt8; keep=false))
    end
end

isinteractive() || perf_revcomp()

