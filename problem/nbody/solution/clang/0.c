/* The Computer Language Benchmarks Game
 * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 *
 * contributed by Miles
 * GCC vector extensions for extra hinting to the compiler
 * fno-math-errno disable error handling use vsqrtpd instead of libm sqrt
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define N 5
#define PI 3.141592653589793
#define SOLAR_MASS (4 * PI * PI)
#define DAYS_PER_YEAR 365.24

// gcc vector extension, used as {w, x, y, z}
// where w is a filler value set to zero
typedef double v4pd __attribute__((vector_size(32)));

// this could probably be slightly better but uses
// less then 0.01% of execution time so isnʼt worth worrying about
double energy(double *m, v4pd *p, v4pd *v) {
    double e = 0.0;

    for (int i = 0; i < N; i++) {
        v4pd t = v[i] * v[i];
        e += 0.5 * m[i] * (t[1] + t[2] + t[3]);

        for (int j = i+1; j < N; j++) {
            v4pd d = p[i] - p[j];
            d = d * d;
            e -= (m[i] * m[j]) / sqrt(d[1] + d[2] + d[3]);
        }
    }
    return e;
}

// emulate builtin to get vsqrtpd
static inline v4pd sqrt_v4pd(v4pd x) {
    v4pd y;
    for (int i = 0; i < 4; i++)
        y[i] = sqrt(x[i]);
    return y;
}

static void advance(int n, double dt, double *m, v4pd *p, v4pd *v) {
    // store intermediate values for easier vectorization
    // array w needs to be aligned to v4pd otherwise it may fail if address is m
isaligned
    // alternative is to use unaligned loads/stores which are slightly slower
    v4pd r[N * (N - 1) / 2];
    double w[N * (N - 1) / 2 + 3] __attribute__((aligned(sizeof(v4pd))));

    // set overhang values to w = dt^(2/5), where dt / (w * sqrt(w)) = w
    // avoid bad values for sqrt and division
    for (int k = 0; k < 3; k++)
        w[N * (N - 1) / 2 + k] = pow(dt, 0.4);

    for (int s = 0; s < n; s++) {
        // iterate over values already loaded in the 2nd loop
        for (int i = 1, k = 0; i < N; i++)
            for (int j = 0; j < i; j++, k++)
                r[k] = p[i] - p[j];

        // compute dot product of each vector with itself
        for (int k = 0; k < N * (N - 1) / 2; k++) {
            v4pd t = r[k] * r[k];
            w[k] = t[1] + t[2] + t[3];
        }

        // split the top and bottom part as two loops for easier vectorization
        // loop already vectorized so no unrolling needed to identify it
        #pragma GCC unroll 0
        for (int k = 0; k < N * (N - 1) / 2; k += 4) {
            v4pd x = *(v4pd*)(w+k);
            x = dt / (x * sqrt_v4pd(x));
            *(v4pd*)(w+k) = x;
        }

        // update velocities
        for (int i = 1, k = 0; i < N; i++)
            for (int j = 0; j < i; j++, k++) {
                v[i] -= r[k] * m[j] * w[k];
                v[j] += r[k] * m[i] * w[k];
            }

        // update positions
        for (int i = 0; i < N; i++)
            p[i] += dt * v[i];
    }
}

int main(int argc, char *argv[]) {
    int n = atoi(argv[1]);

    double m[N];
    v4pd p[N], v[N];

    // sun
    m[0] = SOLAR_MASS;
    p[0] = 0.0 - (v4pd) {};
    v[0] = 0.0 - (v4pd) {};

    // jupiter
    m[1] = 9.54791938424326609e-04 * SOLAR_MASS;
    p[1] = (v4pd) {0.0, 4.84143144246472090e+00, -1.16032004402742839e+00,
        -1.03622044471123109e-01};
    v[1] = (v4pd) {0.0, 1.66007664274403694e-03, 7.69901118419740425e-03,
        -6.90460016972063023e-05} * DAYS_PER_YEAR;

    // saturn
    m[2] = 2.85885980666130812e-04 * SOLAR_MASS;
    p[2] = (v4pd) {0.0, 8.34336671824457987e+00, 4.12479856412430479e+00,
        -4.03523417114321381e-01};
    v[2] = (v4pd) {0.0, -2.76742510726862411e-03, 4.99852801234917238e-03,
        2.30417297573763929e-05} * DAYS_PER_YEAR;

    // uranus
    m[3] = 4.36624404335156298e-05 * SOLAR_MASS;
    p[3] = (v4pd) {0.0, 1.28943695621391310e+01, -1.51111514016986312e+01,
        -2.23307578892655734e-01};
    v[3] = (v4pd) {0.0, 2.96460137564761618e-03, 2.37847173959480950e-03,
        -2.96589568540237556e-05} * DAYS_PER_YEAR;

    // neptune
    m[4] = 5.15138902046611451e-05 * SOLAR_MASS;
    p[4] = (v4pd) {0.0, 1.53796971148509165e+01, -2.59193146099879641e+01,
        1.79258772950371181e-01};
    v[4] = (v4pd) {0.0, 2.68067772490389322e-03, 1.62824170038242295e-03,
        -9.51592254519715870e-05} * DAYS_PER_YEAR;

    // offset momentum
    v4pd o = 0.0 - (v4pd) {};
    for (int i = 0; i < N; i++)
        o += m[i] * v[i];
    v[0] = -o / SOLAR_MASS;

    printf("%.9f\n", energy(m, p, v));
    advance(n, 0.01, m, p, v);
    printf("%.9f\n", energy(m, p, v));

    return 0;
}

