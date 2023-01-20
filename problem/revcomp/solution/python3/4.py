# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# contributed by Jacob Lee, Steven Bethard, et al
# 2to3, fixed by Daniele Varrazzo
# modified by Daniel Nanz

import sys


def show(seq, table=bytes.maketrans(bʼACBDGHKMNSRUTWVYacbdghkmnsrutwvyʼ,
                                    bʼTGVHCDMKNSYAAWBRTGVHCDMKNSYAAWBRʼ),
         write=sys.stdout.buffer.write, nl=bʼ\nʼ):

    [header, s] = seq.split(nl, 1)
    s = s.translate(table, nl)[: : -1]

    write(bʼ>ʼ + header + nl)
    for i in range(0, len(s), 60):
        write(s[i : i + 60] + nl)



def main():

    sys.stdin = sys.stdin.detach()
    seqs = bʼʼ.join([line for line in sys.stdin]).split(bʼ>ʼ)[1 : ]

    for seq in seqs:
        show(seq)


main()

