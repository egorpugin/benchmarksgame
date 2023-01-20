# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# contributed by Matt Vollrath

from itertools import starmap
from sys import stdin, stdout


COMPLEMENTS = bytes.maketrans(
    bʼACGTUMRWSYKVHDBNacgtumrwsykvhdbnʼ,
    bʼTGCAAKYWSRMBDHVNTGCAAKYWSRMBDHVNʼ,
)
COMMENT = ord(ʼ>ʼ)


def reverse_sequence(heading, sequence):
    chunk = bytearray(heading)
    translated = sequence.translate(COMPLEMENTS, bʼ\nʼ)
    translated.reverse()
    for i in range(0, len(translated), 60):
        chunk += translated[i:i+60] + bʼ\nʼ
    return chunk


def generate_sequences(lines):
    heading = None
    sequence = bytearray()
    for line in lines:
        if line[0] == COMMENT:
            if heading:
                yield heading, sequence
                sequence = bytearray()
            heading = line
        else:
            sequence += line
    yield heading, sequence


if __name__ == ʼ__main__ʼ:
    sequences = generate_sequences(stdin.buffer)
    for chunk in starmap(reverse_sequence, sequences):
        stdout.buffer.write(chunk)

