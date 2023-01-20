/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   contributed by Isaac Gouy, RegExp from Jos Hirthʼs program
*/

import ʼdart:ioʼ;

void main() {
  File(ʼ/dev/stdinʼ).readAsString().then((String s) {
    final z = s.replaceAll(RegExp(ʼ>.*\n|\nʼ, multiLine: true), ʼʼ);
    printPatternMatches(z);
    print(ʼ\n${s.length}\n${z.length}\n${magicLengthOf(z)}ʼ);
  });
}

void printPatternMatches(String s) {
  const simple = [
    ʼagggtaaa|tttaccctʼ,
    ʼ[cgt]gggtaaa|tttaccc[acg]ʼ,
    ʼa[act]ggtaaa|tttacc[agt]tʼ,
    ʼag[act]gtaaa|tttac[agt]ctʼ,
    ʼagg[act]taaa|ttta[agt]cctʼ,
    ʼaggg[acg]aaa|ttt[cgt]ccctʼ,
    ʼagggt[cgt]aa|tt[acg]accctʼ,
    ʼagggta[cgt]a|t[acg]taccctʼ,
    ʼagggtaa[cgt]|[acg]ttaccctʼ
  ];

  for (var each in simple) {
    final regex = RegExp(each, multiLine: true);
    print(ʼ$each ${regex.allMatches(s).length}ʼ);
  }
}

int magicLengthOf(String s) {
  const magic = [
    [ʼtHa[Nt]ʼ, ʼ<4>ʼ],
    [ʼaND|caN|Ha[DS]|WaSʼ, ʼ<3>ʼ],
    [ʼa[NSt]|BYʼ, ʼ<2>ʼ],
    [ʼ<[^>]*>ʼ, ʼ|ʼ],
    [ʼ\\|[^|][^|]*\\|ʼ, ʼ-ʼ],
  ];

  for (var each in magic) {
    final regex = RegExp(each.first, multiLine: true);
    s = s.replaceAll(regex, each.last);
  }
  return s.length;
}

