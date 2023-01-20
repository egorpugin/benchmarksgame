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

// based on the spec C# of using 4 functions to compute the spectral norm

// vector of 4 doubles is 4 x 8 bytes = 32 bytes
// vector of 4 ints is 4 x 4 bytes = 16 bytes
typedef double v4pd __attribute__((vector_size(32)));
typedef int v4si __attribute__((vector_size(16)));

// compute values of A 4 at a time instead of 1
static inline v4pd eval_A(v4si i, v4si j) {
    return 1.0 / __builtin_convertvector((i + j) * (i + j + 1) / 2 + i + 1, v4pd
);
}

static void eval_A_times_u(int n, double *u, double *Au) {
    #pragma omp parallel for
    for (int i = 0; i < n; i += 4) {
        // compute output values 4 at a time
        v4pd s = {0.0, 0.0, 0.0, 0.0};
        // generate the 4 indices of i
        v4si p = {i, i + 1, i + 2, i + 3};

        for (int j = 0; j < n; j++) {
            // broadcast the index j as 4 values
            v4si q = {j, j, j, j};
            s += u[j] * eval_A(p, q);
        }

        *(v4pd*)(Au+i) = s;
    }
}

// same as above except indices of A flipped (transposed)
static void eval_At_times_u(int n, double *u, double *Au) {
    #pragma omp parallel for
    for (int i = 0; i < n; i += 4) {
        v4pd s = (v4pd) {0.0, 0.0, 0.0, 0.0};
        v4si p = {i, i + 1, i + 2, i + 3};

        for (int j = 0; j < n; j++) {
            v4si q = {j, j, j, j};
            s += u[j] * eval_A(q, p);
        }

        *(v4pd*)(Au+i) = s;
    }
}

static void eval_AtA_times_u(int n, double *u, double *AtAu) {
    double v[n+3] __attribute__((aligned(sizeof(v4pd))));

    eval_A_times_u(n, u, v);
    eval_At_times_u(n, v, AtAu);
}

int main(int argc, char *argv[]) {
    int n = atoi(argv[1]);

    // overhang of 3 values for computing in strides of 4 incase n % 4 != 0
    // aligned to v4pd to use aligned loads/stores
    double u[n+3] __attribute__((aligned(sizeof(v4pd))));
    double v[n+3] __attribute__((aligned(sizeof(v4pd))));

    for (int i = 0; i < n; i++)
        u[i] = 1.0;

    for (int i = 0; i < 10; i++) {
        eval_AtA_times_u(n, u, v);
        eval_AtA_times_u(n, v, u);
    }

    double uv = 0.0;
    double v2 = 0.0;

    for (int i = 0; i < n; i++) {
        uv += u[i] * v[i];
        v2 += v[i] * v[i];
    }

    printf("%0.9f\n", sqrt(uv / v2));

    return 0;
}

