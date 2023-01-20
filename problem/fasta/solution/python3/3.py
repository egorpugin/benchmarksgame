# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# modified by Ian Osgood
# modified again by Heinrich Acker
# modified by Justin Peel
# Modified by Christopher Sean Forgeron

import sys
import bisect

alu = (
   ʼGGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGʼ
   ʼGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGAʼ
   ʼCCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATʼ
   ʼACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAʼ
   ʼGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGʼ
   ʼAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCʼ
   ʼAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAAʼ)

iub = list(zip(ʼacgtBDHKMNRSVWYʼ, [0.27, 0.12, 0.12, 0.27] + [0.02] * 11))

homosapiens = [
    (ʼaʼ, 0.3029549426680),
    (ʼcʼ, 0.1979883004921),
    (ʼgʼ, 0.1975473066391),
    (ʼtʼ, 0.3015094502008),
]


def make_cumulative(table):
    P = []
    C = []
    prob = 0.
    for char, p in table:
        prob += p
        P += [prob]
        C += [ord(char)]
    return (P, C)


def repeat_fasta(src, n):
    width = 60
    nprint = sys.stdout.buffer.write

    is_trailing_line = False
    count_modifier = 0.0

    len_of_src = len(src)
    ss = src + src + src[:n % len_of_src]
    # CSF - Itʼs faster to work with a bytearray than a string
    s = bytearray(ss, encoding=ʼutf8ʼ)

    if n % width:
        # We donʼt end on a 60 char wide line
        is_trailing_line = True
        count_modifier = 1.0

    # CSF - Here we are stuck with using an int instead of a float for the loop,
    # but testing showed it still to be faster than a for loop
    count = 0
    end = (n / float(width)) - count_modifier
    while count < end:
        i = count*60 % len_of_src
        nprint(s[i:i+60] + bʼ\nʼ)
        count += 1
    if is_trailing_line:
        nprint(s[-(n % width):] + bʼ\nʼ)


def random_fasta(table, n, seed):
    width = 60
    r = range(width)
    bb = bisect.bisect

    # If we donʼt have a multiple of the width, then we will have a trailing
    # line, which needs a slightly different approach
    is_trailing_line = False
    count_modifier = 0.0

    # CSF - nprint allows us to print a bytearray directly to stdout, avoiding
    # some conversion steps along the way, including a call to join
    nprint = sys.stdout.buffer.write
    line = bytearray(width + 1)    # Width of 60 + 1 for the \n char

    probs, chars = make_cumulative(table)

    # pRNG Vars
    im = 139968.0
    #seed = 42.0

    if n % width:
        # We donʼt end on a 60 char wide line
        is_trailing_line = True
        count_modifier = 1.0

    # CSF - Loops with a high iteration count run faster as a while/float loop.
    count = 0.0
    end = (n / float(width)) - count_modifier
    while count < end:
        # CSF - Low iteration count loops may run faster as a for loop.
        for i in r:
            # CSF - Python is faster for all float math than it is for int, on m
y
            # machine at least.
            seed = (seed * 3877.0 + 29573.0) % 139968.0
            # CSF - While real values, not variables are faster for most things,
 on my
            # machine, itʼs faster to have ʼimʼ already in a var
            line[i] = chars[bb(probs, seed / im)]

        line[60] = 10   # End of Line
        nprint(line)
        count += 1.0

    if is_trailing_line:
        for i in range(n % width):
            seed = (seed * 3877.0 + 29573.0) % 139968.0
            line[i] = chars[bb(probs, seed / im)]

        nprint(line[:i+1] + b"\n")

    return seed

def main():
    n = int(sys.argv[1])
    nprint = sys.stdout.buffer.write
    nprint(bʼ>ONE Homo sapiens alu\nʼ)
    repeat_fasta(alu, n * 2)

    # We need to keep track of the state of ʼseedʼ so we pass it in, and return
    # it back so our output can pass the diff test
    nprint(bʼ>TWO IUB ambiguity codes\nʼ)
    seed=random_fasta(iub, n * 3, seed=42.0)

    nprint(bʼ>THREE Homo sapiens frequency\nʼ)
    random_fasta(homosapiens, n * 5, seed)

if __name__ == "__main__":
    main()

