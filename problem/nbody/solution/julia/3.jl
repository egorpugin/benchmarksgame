# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

# contributed by Isaac Yonemoto
# based on the version by Jarret Revels and Alex Arslan

module NBody

using Printf

# Constants
const solar_mass = 4 * pi * pi
const inv_solar_mass =  -1 / solar_mass
const days_per_year = 365.24

const ThreeSpace = NTuple{3, VecElement{Float64}}
Base.zero(::Type{ThreeSpace}) = (VecElement(0.0), VecElement(0.0), VecElement(0.
0))

const threezero = Base.zero(ThreeSpace)

# A heavenly body in the system
mutable struct Body
    pos::ThreeSpace
    vel::ThreeSpace
    mass::Float64
end

function offset_momentum(b::Body, pos :: ThreeSpace)
    local vel::ThreeSpace = b.vel
    b.vel = (VecElement(pos[1].value * inv_solar_mass),
             VecElement(pos[2].value * inv_solar_mass),
             VecElement(pos[3].value * inv_solar_mass))
end

function init_sun(bodies::Vector{Body})
    local p = threezero
    for b in bodies
        vel = b.vel
        p = (VecElement(muladd(vel[1].value, b.mass, p[1].value)),
             VecElement(muladd(vel[2].value, b.mass, p[2].value)),
             VecElement(muladd(vel[3].value, b.mass, p[3].value)))
    end
    offset_momentum(bodies[1], p)
end

function advance(bodies, dt)
    for i = 1:length(bodies)
        for j = i+1:length(bodies)
            bodyipos = bodies[i].pos
            bodyjpos = bodies[j].pos

            dpos::ThreeSpace = (
                VecElement(bodyipos[1].value - bodyjpos[1].value),
                VecElement(bodyipos[2].value - bodyjpos[2].value),
                VecElement(bodyipos[3].value - bodyjpos[3].value)
            )

            dsq::ThreeSpace = (
                VecElement(dpos[1].value * dpos[1].value),
                VecElement(dpos[2].value * dpos[2].value),
                VecElement(dpos[3].value * dpos[3].value)
            )

            tdsq = dsq[1].value + dsq[2].value + dsq[3].value
            distance = sqrt(tdsq)
            mag = dt / (tdsq * distance)
            imassmag = bodies[i].mass * mag
            negjmassmag = - bodies[j].mass * mag

            bodyivel = bodies[i].vel
            bodyjvel = bodies[j].vel

            bodyivel::ThreeSpace = (
                VecElement(muladd(dpos[1].value, negjmassmag, bodyivel[1].value)
),
                VecElement(muladd(dpos[2].value, negjmassmag, bodyivel[2].value)
),
                VecElement(muladd(dpos[3].value, negjmassmag, bodyivel[3].value)
)
            )

            bodyjvel::ThreeSpace = (
                VecElement(muladd(dpos[1].value, imassmag, bodyjvel[1].value)),
                VecElement(muladd(dpos[2].value, imassmag, bodyjvel[2].value)),
                VecElement(muladd(dpos[3].value, imassmag, bodyjvel[3].value))
            )

            bodies[i].vel = bodyivel
            bodies[j].vel = bodyjvel
        end
    end

    for b in bodies
        bodypos = b.pos
        bodyvel = b.vel
        newbodypos::ThreeSpace = (
            VecElement(muladd(bodyvel[1].value, dt, bodypos[1].value)),
            VecElement(muladd(bodyvel[2].value, dt, bodypos[2].value)),
            VecElement(muladd(bodyvel[3].value, dt, bodypos[3].value))
        )
        b.pos = newbodypos
    end
end

function energy(bodies::Vector{Body})
    local e::Float64 = 0.0
    for i = 1:length(bodies)
        vel = bodies[i].vel
        pos = bodies[i].pos

        e += 0.5 * bodies[i].mass * (vel[1].value^2 + vel[2].value^2 + vel[3].va
lue^2)

        for j = i+1:length(bodies)
            posj = bodies[j].pos

            dx = pos[1].value - posj[1].value
            dy = pos[2].value - posj[2].value
            dz = pos[3].value - posj[3].value

            distance = sqrt(dx^2 + dy^2 + dz^2)
            e -= (bodies[i].mass * bodies[j].mass) / distance
        end
    end
    e
end

function initbody(a,b,c,d,e,f,g)
    Body((
        VecElement(a),
        VecElement(b),
        VecElement(c)
    ),(
        VecElement(d),
        VecElement(e),
        VecElement(f)
    ),
    g)
end

function perf_nbody(N::Int=1000)
    jupiter = initbody( 4.84143144246472090e+00,                  # x
                   -1.16032004402742839e+00,                  # y
                   -1.03622044471123109e-01,                  # z
                   1.66007664274403694e-03 * days_per_year,   # vx
                   7.69901118419740425e-03 * days_per_year,   # vy
                   -6.90460016972063023e-05 * days_per_year,  # vz
                   9.54791938424326609e-04 * solar_mass)      # mass

    saturn = initbody( 8.34336671824457987e+00,
                  4.12479856412430479e+00,
                  -4.03523417114321381e-01,
                  -2.76742510726862411e-03 * days_per_year,
                  4.99852801234917238e-03 * days_per_year,
                  2.30417297573763929e-05 * days_per_year,
                  2.85885980666130812e-04 * solar_mass)

    uranus = initbody( 1.28943695621391310e+01,
                  -1.51111514016986312e+01,
                  -2.23307578892655734e-01,
                  2.96460137564761618e-03 * days_per_year,
                  2.37847173959480950e-03 * days_per_year,
                  -2.96589568540237556e-05 * days_per_year,
                  4.36624404335156298e-05 * solar_mass)

    neptune = initbody( 1.53796971148509165e+01,
                   -2.59193146099879641e+01,
                   1.79258772950371181e-01,
                   2.68067772490389322e-03 * days_per_year,
                   1.62824170038242295e-03 * days_per_year,
                   -9.51592254519715870e-05 * days_per_year,
                   5.15138902046611451e-05 * solar_mass)

    sun = initbody(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, solar_mass)

    bodies = [sun, jupiter, saturn, uranus, neptune]

    init_sun(bodies)

    @printf("%.9f\n", energy(bodies))

    for i = 1:N
        advance(bodies, 0.01)
    end
    @printf("%.9f\n", energy(bodies))
end

end # module

n = parse(Int,ARGS[1])
NBody.perf_nbody(n)

