/* The Computer Language Benchmarks Game

   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   Contributed by Philip Rogers
   Based on a javascript implementation by Jesse Millikan and Matt Baker
   + null safety
*/

import ʼdart:ioʼ;
import ʼdart:collectionʼ;

String readLine() => stdin.readLineSync() ?? ʼ>>>>>>ʼ;

String readInput() {
  while (readLine().substring(0, 6) != ʼ>THREEʼ);

  final lines = <String>[];
  String line = readLine();
  while (line[0] != ʼ>ʼ) {
    lines.add(line);
    line = readLine();
  }
  ;
  return lines.join(ʼʼ).toUpperCase();
}

HashMap<String, int> frequency(String sequence, int length) {
  HashMap<String, int> freq = new HashMap<String, int>();
  int n = sequence.length - length + 1;
  String sub;
  for (int i = 0; i < n; i++) {
    sub = sequence.substring(i, i + length);
    if (freq.containsKey(sub))
      freq[sub] = (freq[sub] ?? 0) + 1;
    else
      freq[sub] = 1;
  }
  return freq;
}

void sort(String sequence, int length) {
  HashMap<String, int> freq = frequency(sequence, length);
  List<String> keys = freq.keys.toList();
  int n = sequence.length - length + 1;

  keys.sort((a, b) {
    int _a = freq[a] ?? 0;
    int _b = freq[b] ?? 0;
    return _b - _a;
  });

  for (String key in keys) {
    String count = ((freq[key] ?? 0) * 100 / n).toStringAsFixed(3);
    print(ʼ$key $countʼ);
  }
  print(ʼʼ);
}

void find(String sequence, String string) {
  HashMap<String, int> freq = frequency(sequence, string.length);
  print(ʼ${(freq[string])}\t$stringʼ);
}

void main(args) {
  String sequence = readInput();

  sort(sequence, 1);
  sort(sequence, 2);
  find(sequence, ʼGGTʼ);
  find(sequence, ʼGGTAʼ);
  find(sequence, ʼGGTATTʼ);
  find(sequence, ʼGGTATTTTAATTʼ);
  find(sequence, ʼGGTATTTTAATTTATAGTʼ);
}

