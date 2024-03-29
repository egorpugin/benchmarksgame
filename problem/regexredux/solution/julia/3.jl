# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# contributed by Daniel Jones
# fixed by David Campbell
# modified by Jarret Revels, Alex Arslan, Yichao Yu
# made (slightly) multi-threaded by Adam Beckmeyer

# Speedup from pcre_jit_match instead of pcre_match in Base. Maybe 2%.
const PCRE = Base.PCRE
function PCRE.exec(re, subject, offset, options, match_data)
    0 <= ccall((:pcre2_jit_match_8, PCRE.PCRE_LIB), Cint,
               (Ptr{Cvoid}, Ptr{UInt8}, Csize_t, Csize_t, Cuint, Ptr{Cvoid}, Ptr
{Cvoid}),
               re, subject, sizeof(subject), offset, options, match_data,
               PCRE.get_local_match_context())
end

const variants = (
      r"agggtaaa|tttaccct",
      r"[cgt]gggtaaa|tttaccc[acg]",
      r"a[act]ggtaaa|tttacc[agt]t",
      r"ag[act]gtaaa|tttac[agt]ct",
      r"agg[act]taaa|ttta[agt]cct",
      r"aggg[acg]aaa|ttt[cgt]ccct",
      r"agggt[cgt]aa|tt[acg]accct",
      r"agggta[cgt]a|t[acg]taccct",
      r"agggtaa[cgt]|[acg]ttaccct"
)

const subs = (
    (r"tHa[Nt]", "<4>"),
    (r"aND|caN|Ha[DS]|WaS", "<3>"),
    (r"a[NSt]|BY", "<2>"),
    (r"<[^>]*>", "|"),
    (r"\|[^|][^|]*\|", "-")
)

function perf_regex_dna(io, out)
    seq = read(io, String)
    # lastindex is faster than length and correct with ascii input. 4% speedup.
    l1 = lastindex(seq)

    seq = replace(seq, r">.*\n|\n" => "")
    l2 = lastindex(seq)

    w = Threads.@spawn for v in variants
        c = 0
        for _ in eachmatch(v, seq)
            c += 1
        end
        println(out, v.pattern, ' ', c)
    end

    repseq = seq
    for (u, v) in subs
        repseq = replace(repseq, u => v)
    end


    wait(w)
    write(out, '\n', string(l1), '\n', string(l2), '\n', string(lastindex(repseq
)), '\n')
end

isinteractive() || perf_regex_dna(stdin, stdout)

