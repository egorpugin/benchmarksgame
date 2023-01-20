# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# Contributed by Andrei Fomiga, Stefan Karpinski, Viral B. Shah, Jeff
# Bezanson, smallnamespaces, and Adam Beckmeyer.
#
# This implementation was specifically written so that for-loops (and
# calls to ntuple) have integer-literal ranges as their iterable. This
# makes the number of loop iterations available as a compile-time
# constant, so llvm can unroll them. To convince llvm to do so, this
# script should be run with the evironment variable:
# JULIA_LLVM_ARGS=ʼ-unroll-threshold=500ʼ.

using Printf

const SOLAR_MASS = 4 * pi * pi
const DAYS_PER_YEAR = 365.24
# Precalculate the pairs of bodies that must be compared so that it
# doesnʼt have to be done each loop.
const PAIRS = Tuple((i, j) for i=1:4 for j=i+1:5)
const V = VecElement

# Use a struct instead of mutable struct since a struct can be stored
# inline in an array avoiding the overhead of following a pointer
struct Body
    x::NTuple{3,Float64}
    v::NTuple{3,Float64}
    m::Float64
end

function init_sun(bodies)
    p = (0.0, 0.0, 0.0)
    for b in bodies
        p = p .- b.v .* b.m
    end
    Body((0.0, 0.0, 0.0), p ./ SOLAR_MASS, SOLAR_MASS)
end

# Advance all bodies in the system by one timestep of 0.01. This
# function always uses a timestep of 0.01 and assumes that there are
# exactly 5 bodies in the system.
@inline function advance!(bodies)
    # Δx holds the difference in position between bodies for each
    # interaction. ntuple(f, Val(10)) is used instead of ntuple(f, 10)
    # so that the results of the function call are type-stable. 10 is
    # the number of pairs of bodies.
    Δx = ntuple(Val(10)) do k
        @inbounds bodies[PAIRS[k][1]].x .- bodies[PAIRS[k][2]].x
    end

    # This function calculates the magnitude of forces between two
    # pairs of bodies (determined by k).  It canʼt be written using
    # do-notation on the ntuple call below because it must be inlined
    # for performance, and Julia doesnʼt seem to have syntax to force
    # inlining with do-notation functions.
    @inline function magnitude(k)
        dx1 = @inbounds Δx[2k-1]
        dx2 = @inbounds Δx[2k]

        dsq1 = sum(dx1 .* dx1)
        dsq2 = sum(dx2 .* dx2)

        # Float64 sqrt is relatively expensive. As an approximation, we
        # use the SSE single-precision reciprocal square root
        # approximation along with two iterations of the Newton-Raphson
        # method.
        v = Base.llvmcall(("""
declare <4 x float> @llvm.x86.sse.rsqrt.ps(<4 x float>)
declare <4 x float> @llvm.x86.sse2.cvtpd2ps(<2 x double>)
        """, """
%2 = call <4 x float> @llvm.x86.sse2.cvtpd2ps(<2 x double> %0)
%3 = call <4 x float> @llvm.x86.sse.rsqrt.ps(<4 x float> %2)
%4 = shufflevector <4 x float> %3, <4 x float> undef, <2 x i32> <i32 0, i32 1>
%5 = fpext <2 x float> %4 to <2 x double>
ret <2 x double> %5
        """), NTuple{2,V{Float64}}, Tuple{NTuple{2,V{Float64}}}, (V(dsq1), V(dsq
2)))
        rd1, rd2 = @inbounds v[1].value, v[2].value

        # Two iterations of Newton-Raphson method
        for _=1:2
            rd1 = 1.5rd1 - 0.5dsq1 * rd1 * (rd1 * rd1)
            rd2 = 1.5rd2 - 0.5dsq2 * rd2 * (rd2 * rd2)
        end

        0.01rd1 * (rd1 * rd1), 0.01rd2 * (rd2 * rd2)
    end

    # Call magnitude 5 times to obtain an NTuple{5,NTuple{2,Float64}}
    # of the force-magnitudes for pairs of bodies.
    mags2 = ntuple(magnitude, Val(5))
    # This very ugly call flattens the results of the above call to
    # ntuple for convenience and so less index-arithmetic is necessary
    # in the loop below.
    mags = ntuple(k-> mags2[(k+1)÷2][iseven(k)+1], Val(10))

    # Use the inter-body force magnitudes to update the velocities of
    # all bodies.
    k = 1
    @inbounds for i=1:4
        vi = bodies[i].v
        mi = bodies[i].m

        for j=i+1:5
            bj = bodies[j]

            vi = vi .- Δx[k] .* (mags[k] * bj.m)
            bodies[j] = Body(bj.x, bj.v .+ Δx[k] .* (mags[k] * mi), bj.m)
            k += 1
        end
        bodies[i] = Body(bodies[i].x, vi, mi)
    end

    # Advance body positions using the updated velocities.
    @inbounds for i=1:5
        bi = bodies[i]
        bodies[i] = Body(bi.x .+ bi.v .* 0.01, bi.v, bi.m)
    end
end

# Total energy of the system
function energy(bodies)
    e = 0.0
    # Kinetic energy of bodies
    @inbounds for b in bodies
        e += 0.5b.m * sum(b.v .* b.v)
    end

    # Potential energy between body i and body j
    @inbounds for (i, j) in PAIRS
        Δx = bodies[i].x .- bodies[j].x
        e -= bodies[i].m * bodies[j].m / √sum(Δx .* Δx)
    end
    e
end

# Mutate bodies array according to symplectic integrator in advance!
# for n iterations.
nbody!(bodies, n) = for i=1:n
    advance!(bodies)
end

# Doing the allocation of the Vector{Body} as a global constant
# instead of within the nbody! function speeds up inference
# considerably. Inference takes less than 60% of the time it would
# otherwise for an overall speedup of 2%-3%.
const bodies = [
    # Jupiter
    Body(( 4.84143144246472090e+0,                # x
          -1.16032004402742839e+0,                # y
          -1.03622044471123109e-1),               # z
         ( 1.66007664274403694e-3DAYS_PER_YEAR,   # vx
           7.69901118419740425e-3DAYS_PER_YEAR,   # vy
          -6.90460016972063023e-5DAYS_PER_YEAR),  # vz
           9.54791938424326609e-4SOLAR_MASS)      # mass
    # Saturn
    Body(( 8.34336671824457987e+0,
           4.12479856412430479e+0,
          -4.03523417114321381e-1),
         (-2.76742510726862411e-3DAYS_PER_YEAR,
           4.99852801234917238e-3DAYS_PER_YEAR,
           2.30417297573763929e-5DAYS_PER_YEAR),
           2.85885980666130812e-4SOLAR_MASS)
    # Uranus
    Body(( 1.28943695621391310e+1,
          -1.51111514016986312e+1,
          -2.23307578892655734e-1),
         ( 2.96460137564761618e-3DAYS_PER_YEAR,
           2.37847173959480950e-3DAYS_PER_YEAR,
          -2.96589568540237556e-5DAYS_PER_YEAR),
           4.36624404335156298e-5SOLAR_MASS)
    # Neptune
    Body(( 1.53796971148509165e+1,
          -2.59193146099879641e+1,
           1.79258772950371181e-1),
         ( 2.68067772490389322e-3DAYS_PER_YEAR,
           1.62824170038242295e-3DAYS_PER_YEAR,
          -9.51592254519715870e-5DAYS_PER_YEAR),
           5.15138902046611451e-5SOLAR_MASS)
]
push!(bodies, init_sun(bodies))

if !isinteractive()
    @printf("%.9f\n", energy(bodies))
    nbody!(bodies, parse(Int, ARGS[1]))
    @printf("%.9f\n", energy(bodies))
end

