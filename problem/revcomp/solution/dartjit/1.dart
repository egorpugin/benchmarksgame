/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   contributed by Isaac Gouy based on Jeremy Zerfasʼs #6 C program
*/

import ʼdart:ioʼ;
import ʼdart:typed_dataʼ;

final nl = ʼ\nʼ.codeUnitAt(0);
final def = ʼ>ʼ.codeUnitAt(0); // Definition-line.
final lut = codeComplementLookupTable();

// READ_SIZE probably 64x1024 ?

void main() async {
  var sequence = BytesBuilder(copy: false);
  await for (var block in stdin) {
    var sequenceStart = block.indexOf(def);
    if (sequenceStart >= 0) {
      if (sequence.length > 0) {
        sequence.add(block.sublist(0, sequenceStart));
        reverseAndComplement(sequence.takeBytes());
        sequence.add(block.sublist(sequenceStart, block.length));
        block = <int>[];
      }
    }
    sequence.add(block);
  }
  reverseAndComplement(sequence.takeBytes());
}

void reverseAndComplement(List<int> sequence) {
  var frontPos = sequence.indexOf(nl) + 1;
  var backPos = sequence.length - 1;
  while (frontPos <= backPos && sequence[frontPos] == nl) frontPos++;
  while (frontPos <= backPos && sequence[backPos] == nl) backPos--;

  while (frontPos <= backPos) {
    var tmp = lut[sequence[frontPos]];
    sequence[frontPos] = lut[sequence[backPos]];
    sequence[backPos] = tmp;
    while (sequence[++frontPos] == nl);
    while (sequence[--backPos] == nl);
  }
  stdout.add(sequence);
}

Uint8List codeComplementLookupTable() {
  final code =       ʼABCDGHKMNRSTUVWYʼ.codeUnits;
  final complement = ʼTVGHCDMKNYSAABWRʼ.codeUnits;
  final lowercaseOffset = ʼaʼ.codeUnitAt(0) - ʼAʼ.codeUnitAt(0);
  final t = Uint8List.fromList(List.generate(128, (int i) => i));
  for (var i = 0; i < code.length; i++) {
    t[code[i]] = complement[i];
    t[code[i] + lowercaseOffset] = complement[i];
  }
  return t;
}


