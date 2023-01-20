# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# modified by Ian Osgood
# modified again by Heinrich Acker
# modified by Justin Peel
# 2to3

import sys, bisect

alu = (
   ʼGGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGʼ
   ʼGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGAʼ
   ʼCCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATʼ
   ʼACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAʼ
   ʼGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGʼ
   ʼAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCʼ
   ʼAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAAʼ)

iub = list(zip(ʼacgtBDHKMNRSVWYʼ, [0.27, 0.12, 0.12, 0.27] + [0.02]*11))

homosapiens = [
    (ʼaʼ, 0.3029549426680),
    (ʼcʼ, 0.1979883004921),
    (ʼgʼ, 0.1975473066391),
    (ʼtʼ, 0.3015094502008),
]


def genRandom(ia = 3877, ic = 29573, im = 139968):
    seed = 42
    imf = float(im)
    while 1:
        seed = (seed * ia + ic) % im
        yield seed / imf

Random = genRandom()

def makeCumulative(table):
    P = []
    C = []
    prob = 0.
    for char, p in table:
        prob += p
        P += [prob]
        C += [char]
    return (P, C)

def repeatFasta(src, n):
    width = 60
    r = len(src)
    s = src + src + src[:n % r]
    for j in range(n // width):
        i = j*width % r
        print(s[i:i+width])
    if n % width:
        print(s[-(n % width):])

def randomFasta(table, n):
    width = 60
    r = range(width)
    gR = Random.__next__
    bb = bisect.bisect
    jn = ʼʼ.join
    probs, chars = makeCumulative(table)
    for j in range(n // width):
        x = jn([chars[bb(probs, gR())] for i in r])
        print(x)
    if n % width:
        print(jn([chars[bb(probs, gR())] for i in range(n % width)]))

def main():
    n = int(sys.argv[1])

    print(ʼ>ONE Homo sapiens aluʼ)
    repeatFasta(alu, n*2)

    print(ʼ>TWO IUB ambiguity codesʼ)
    randomFasta(iub, n*3)

    print(ʼ>THREE Homo sapiens frequencyʼ)
    randomFasta(homosapiens, n*5)

main()


