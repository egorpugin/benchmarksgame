/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   regex-dna program contributed by Jos Hirth, based on the JavaScript version
     which was created by Jesse Millikan, jose fco. gonzalez, and Matthew Wilson

   converted from regex-dna program
*/

import ʼdart:ioʼ;
import ʼdart:convertʼ;

void main() {
  var text = StringBuffer();
  var src = stdin.transform(Utf8Decoder()).transform(LineSplitter());

  src.listen((line) {
    text.write(line);
    text.write(ʼ\nʼ);
  }, onDone: () {
    regexAllTheThings(text.toString());
  });
}

void regexAllTheThings(String fullText) {
  var lengthA, lengthB, lengthC, regexp, replacements;

  regexp = (() {
    var pattern = [
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
    var regexp = [];
    for (var p in pattern) {
      regexp.add(RegExp(p, caseSensitive: false));
    }
    return regexp;
  }());

  replacements = [
    ʼtHa[Nt]ʼ,
    ʼ<4>ʼ,
    ʼaND|caN|Ha[DS]|WaSʼ,
    ʼ<3>ʼ,
    ʼa[NSt]|BYʼ,
    ʼ<2>ʼ,
    ʼ<[^>]*>ʼ,
    ʼ|ʼ,
    ʼ\\|[^|][^|]*\\|ʼ,
    ʼ-ʼ
  ];

  lengthA = fullText.length;

  fullText = fullText.replaceAll(RegExp(ʼ^>.*\n|\nʼ, multiLine: true),
      ʼʼ); // still ridiculously slow with r21658

  lengthB = fullText.length;

  for (var i = 0; i < regexp.length; ++i) {
    print(ʼ${regexp[i].pattern} ${regexp[i].allMatches(fullText).length}ʼ);
  }

  for (var i = -1; i < replacements.length - 1;) {
    fullText =
        fullText.replaceAll(RegExp(replacements[++i]), replacements[++i]);
  }

  lengthC = fullText.length;

  print(ʼ\n$lengthA\n$lengthB\n$lengthCʼ);
}

