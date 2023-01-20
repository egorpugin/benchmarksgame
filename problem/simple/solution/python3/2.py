# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
# Written for Python 2
# contributed by Brent Fulgham

import sys

def main():
    bit_num = 0
    byte_acc = 0
    iter = 50
    limit2 = 4.
    w = h = int(sys.argv[1])

    sys.stdout.write(f'P4\n{w} {h}\n'); sys.stdout.flush()

    for y in range(h):
        fy = 2j * y / h - 1j
        for x in range(w):
            z = 0j
            c = 2. * x / w - 1.5 + fy

            byte_acc <<= 1
            bit_num += 1

            for i in range(iter):
                z = z * z + c
                if z.real * z.real + z.imag * z.imag > limit2:
                    break
            else:
                byte_acc += 1

            if bit_num == 8:
                # Python 2.7 sys.stdout.write(chr(byte_acc))
                sys.stdout.buffer.write(bytes([byte_acc]))
                byte_acc = bit_num = 0
            elif x == w - 1:
                byte_acc = byte_acc << (8 - w % 8)
                # Python 2.7 sys.stdout.write(chr(byte_acc))
                sys.stdout.buffer.write(bytes([byte_acc]))
                byte_acc = bit_num = 0

main()

