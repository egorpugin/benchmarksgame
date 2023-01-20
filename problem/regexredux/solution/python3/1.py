# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# regex-dna program contributed by Dominique Wahli
# 2to3
# mp by Ahmad Syukri
# modified by Justin Peel
# converted from regex-dna program

from sys import stdin
from re import sub, findall
from multiprocessing import Pool

def init(arg):
    global seq
    seq = arg

def var_find(f):
    return len(findall(f, seq))

def main():
    seq = stdin.read()
    ilen = len(seq)

    seq = sub(ʼ>.*\n|\nʼ, ʼʼ, seq)
    clen = len(seq)

    pool = Pool(initializer = init, initargs = (seq,))

    variants = (
          ʼagggtaaa|tttaccctʼ,
          ʼ[cgt]gggtaaa|tttaccc[acg]ʼ,
          ʼa[act]ggtaaa|tttacc[agt]tʼ,
          ʼag[act]gtaaa|tttac[agt]ctʼ,
          ʼagg[act]taaa|ttta[agt]cctʼ,
          ʼaggg[acg]aaa|ttt[cgt]ccctʼ,
          ʼagggt[cgt]aa|tt[acg]accctʼ,
          ʼagggta[cgt]a|t[acg]taccctʼ,
          ʼagggtaa[cgt]|[acg]ttaccctʼ)
    for f in zip(variants, pool.imap(var_find, variants)):
        print(f[0], f[1])

    subst = {
          ʼtHa[Nt]ʼ : ʼ<4>ʼ, ʼaND|caN|Ha[DS]|WaSʼ : ʼ<3>ʼ, ʼa[NSt]|BYʼ : ʼ<2>ʼ,
          ʼ<[^>]*>ʼ : ʼ|ʼ, ʼ\\|[^|][^|]*\\|ʼ : ʼ-ʼ}
    for f, r in list(subst.items()):
        seq = sub(f, r, seq)

    print()
    print(ilen)
    print(clen)
    print(len(seq))

if __name__=="__main__":
    main()

