#=
The Computer Language Benchmarks Game
https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
contributed by Adam Beckmeyer with help from Vincent Yu
=#

A(i, j) = (i + j - 2) * (i + j - 1) / 2 + i
At(i, j) = A(j, i)

function Av!(f, v, out)
    n = length(v)
    # Threads.@threads has lower overhead than Threads.@spawn
    Threads.@threads :static for i=1:n
        x1 = x2 = 0.0
        # If we used the @simd macro instead of manually iterating by 2s, the co
mpiler would
        # emit instructions using the ymm registers instead of xmm which appears
 to be
        # slower on ivybridge cpus
        @inbounds for j=1:2:n
            # Manually convert indices to Float64 so that arithmetic in function
 f can be
            # carried out using vectorized  floating point arithmetic
            x1 += v[j] / f(Float64(i), Float64(j))
            x2 += v[j+1] / f(Float64(i), Float64(j+1))
        end
        @inbounds out[i] = x1 + x2
    end
end

function main(n)
    # This program is not compatible with odd values of n
    isodd(n) && (n += 1)

    u = ones(Float64, n)
    v = Vector{Float64}(undef, n)
    # temporary working vector w
    w = Vector{Float64}(undef, n)

    for _=1:10
        Av!(A, u, w)
        Av!(At, w, v)
        Av!(A, v, w)
        Av!(At, w, u)
    end

    uv = vv = 0.0
    @inbounds for i=1:n
        uv += u[i] * v[i]
        vv += v[i] * v[i]
    end
    sqrt(uv / vv)
end

# The expanded form of Printf.@printf macro takes a significant time to compile,
 accounting
# for 25% of the total program runtime. Base.Ryu.writefixed(::Float64, ::Int) sh
ould already
# be compiled into the default system image.
isinteractive() || println(Base.Ryu.writefixed(main(parse(Int, ARGS[1])), 9))

