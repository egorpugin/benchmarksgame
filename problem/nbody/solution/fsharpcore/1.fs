// The Computer Language Benchmarks Game
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
//
// contributed by Valentin Kraevskiy
// modified by Peter Kese
// rewritten to be less "advanced" and similar to the OCAML implementation by Ph
illip Carter

open System

let solar_mass = 4. * Math.PI * Math.PI
let days_per_year = 365.24

type Planet =
    { mutable X: float
      mutable Y: float
      mutable Z: float
      mutable VX: float
      mutable VY: float
      mutable VZ: float
      Mass: float }

let advance bodies dt =
    let n = Array.length bodies - 1

    for i = 0 to n do
        let b = bodies.[i]
        for j = i+1 to n do
            let bʼ = bodies.[j]
            let dx = b.X - bʼ.X
            let dy = b.Y - bʼ.Y
            let dz = b.Z - bʼ.Z
            let dist2 = dx * dx + dy * dy + dz * dz
            let mag = dt / (dist2 * sqrt(dist2))

            b.VX <- b.VX - dx * bʼ.Mass * mag
            b.VY <- b.VY - dy * bʼ.Mass * mag
            b.VZ <- b.VZ - dz * bʼ.Mass * mag

            bʼ.VX <- bʼ.VX + dx * b.Mass * mag
            bʼ.VY <- bʼ.VY + dy * b.Mass * mag
            bʼ.VZ <- bʼ.VZ + dz * b.Mass * mag

    for i = 0 to n do
        let b = bodies.[i]
        b.X <- b.X + dt * b.VX
        b.Y <- b.Y + dt * b.VY
        b.Z <- b.Z + dt * b.VZ

let energy bodies =
    let mutable e = 0.0
    let n = Array.length bodies - 1
    for i = 0 to n do
        let b = bodies.[i]
        e <- e + 0.5 * b.Mass * (b.VX * b.VX + b.VY * b.VY + b.VZ * b.VZ)
        for j = i+1 to n do
            let bʼ = bodies.[j]
            let dx = b.X - bʼ.X
            let dy = b.Y - bʼ.Y
            let dz = b.Z - bʼ.Z
            let distance = sqrt(dx * dx + dy * dy + dz * dz)
            e <- e - (b.Mass * bʼ.Mass) / distance
    e

let offset_momentum bodies =
    let mutable px = 0.0
    let mutable py = 0.0
    let mutable pz = 0.0

    for i = 0 to Array.length bodies - 1 do
        px <- px + bodies.[i].VX * bodies.[i].Mass
        py <- py + bodies.[i].VY * bodies.[i].Mass
        pz <- pz + bodies.[i].VZ * bodies.[i].Mass

    bodies.[0].VX <- -px / solar_mass
    bodies.[0].VY <- -py / solar_mass
    bodies.[0].VZ <- -pz / solar_mass

let jupiter =
    { X = 4.84143144246472090e+00
      Y = -1.16032004402742839e+00
      Z = -1.03622044471123109e-01
      VX = 1.66007664274403694e-03 * days_per_year
      VY = 7.69901118419740425e-03 * days_per_year
      VZ = -6.90460016972063023e-05 * days_per_year
      Mass = 9.54791938424326609e-04 * solar_mass }

let saturn =
    { X = 8.34336671824457987e+00
      Y = 4.12479856412430479e+00
      Z = -4.03523417114321381e-01
      VX = -2.76742510726862411e-03 * days_per_year
      VY = 4.99852801234917238e-03 * days_per_year
      VZ = 2.30417297573763929e-05 * days_per_year
      Mass = 2.85885980666130812e-04 * solar_mass }

let uranus =
    { X = 1.28943695621391310e+01
      Y = -1.51111514016986312e+01
      Z = -2.23307578892655734e-01
      VX = 2.96460137564761618e-03 * days_per_year
      VY = 2.37847173959480950e-03 * days_per_year
      VZ = -2.96589568540237556e-05 * days_per_year
      Mass = 4.36624404335156298e-05 * solar_mass }

let neptune =
    { X = 1.53796971148509165e+01
      Y = -2.59193146099879641e+01
      Z = 1.79258772950371181e-01
      VX = 2.68067772490389322e-03 * days_per_year
      VY = 1.62824170038242295e-03 * days_per_year
      VZ = -9.51592254519715870e-05 * days_per_year
      Mass = 5.15138902046611451e-05 * solar_mass   }

let sun =
    { X = 0.0
      Y = 0.0
      Z = 0.0
      VX = 0.0
      VY = 0.0
      VZ = 0.0
      Mass = solar_mass }

let bodies = [| sun; jupiter; saturn; uranus; neptune |]

[<EntryPoint>]
let main args =
    let n = if Array.isEmpty args then 20_000_000 else int args.[0]
    let e0 = energy bodies
    offset_momentum bodies

    for _ = 1 to n do
        advance bodies 0.01

    let e1 = energy bodies
    printf "%.9f\n%.9f\n" e0 e1
    0

