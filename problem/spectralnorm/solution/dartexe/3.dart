/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   contributed by Isaac Gouy
   (start end ranges from Andrey Filatkin's JavaScript #6 program)
*/

import 'dart:math';
import 'dart:typed_data';
import 'dart:isolate';
import 'dart:async';
import 'dart:io';

double a(int i, int j) {
  return 1.0 / (((i + j) * (i + j + 1) >> 1) + i + 1);
}

Float64List auFromTo(int start, int end, Float64List u) {
  final n = u.length;
  final w = new Float64List(n);
  for (int i = start; i < end; ++i) {
    double t = 0.0;
    for (int j = 0; j < n; ++j) {
      t += u[j] * a(i, j);
    }
    w[i] = t;
  }
  return w;
}

Float64List atuFromTo(int start, int end, Float64List u) {
  final n = u.length;
  final w = new Float64List(n);
  for (int i = start; i < end; ++i) {
    double t = 0.0;
    for (int j = 0; j < n; ++j) {
      t += u[j] * a(j, i);
    }
    w[i] = t;
  }
  return w;
}

// same w out reference style as sequential program
Future<void> au(Float64List u, Float64List w) async {
  await mapToIsolates(false, u, w);
}

// same w out reference style as sequential program
Future<void> atu(Float64List u, Float64List w) async {
  await mapToIsolates(true, u, w);
}

// same w out reference style as sequential program
Future<void> atAu(Float64List u, Float64List v, Float64List w) async {
  await au(u, w);
  await atu(w, v);
}

Future<void> spectralNorm(int n) async {
  var u = new Float64List(n)..fillRange(0, n, 1.0),
      v = new Float64List(n),
      w = new Float64List(n),
      vBv = 0.0,
      vv = 0.0;
  // same w out reference style as sequential program
  for (int i = 0; i < 10; ++i) {
    await atAu(u, v, w);
    await atAu(v, u, w);
  }
  for (int i = 0; i < n; ++i) {
    vBv += u[i] * v[i];
    vv += v[i] * v[i];
  }
  print(sqrt(vBv / vv).toStringAsFixed(9));
}

var nCpus = 1;
final completers = <Completer>[];
final ports = <SendPort>[];
ReceivePort mainIsolate = ReceivePort();
final zeroList = new Float64List(0);
Float64List wOutReference = zeroList;

void main(List<String> args) async {
  final n = (args.length > 0) ? int.parse(args[0]) : 100;
  nCpus = Platform.numberOfProcessors;
  setStartEndRanges(n);
  await spawnIsolates();
  mainIsolate.listen((dynamic m) {
    if (m is Message) {
      // copy partial auFromTo / atuFromTo result to w out reference
      List.copyRange(wOutReference, m.start, m.data, m.start, m.end);
      completers.removeLast().complete();
    }
  });
  await spectralNorm(n);
  mainIsolate.close();
}

final startEndRanges = <Message>[];

void setStartEndRanges(int n) {
  final size = (n / nCpus).ceil();
  for (var i = 0; i < nCpus; i++) {
    final start = i * size;
    var end = start + size;
    if (end > n) {
      end = n;
    }
    startEndRanges.add(Message(start, end));
  }
}

Future<void> spawnIsolates() async {
  ReceivePort replyPort = ReceivePort();
  final barrier = <Future>[];
  var i = nCpus;
  while (i-- > 0) {
    final c = Completer<dynamic>();
    completers.add(c);
    barrier.add(c.future);
    Isolate.spawn(requestReply, replyPort.sendPort);
  }
  replyPort.listen((dynamic p) {
    ports.add(p as SendPort);
    completers.removeLast().complete();
  });
  await Future.wait<void>(barrier);
  replyPort.close();
}

void requestReply(SendPort p) {
  ReceivePort requestPort = ReceivePort();
  p.send(requestPort.sendPort);
  requestPort.listen((dynamic m) {
    // update data and send the same message back
    if (m is Message) {
      if (m.shouldTranspose) {
        m.data = atuFromTo(m.start, m.end, m.data);
      } else {
        m.data = auFromTo(m.start, m.end, m.data);
      }
      m.replyTo.send(m);
    }
  });
}

var next = 0;

Future<void> mapToIsolates(
    bool shouldTranspose, Float64List u, Float64List w) async {
  wOutReference = w; // same out reference style as sequential program
  final barrier = <Future>[];
  startEndRanges.forEach((m) {
    m.shouldTranspose = shouldTranspose;
    m.data = u;
    final c = Completer<dynamic>();
    completers.add(c);
    barrier.add(c.future);
    next = (next + 1) % ports.length;
    ports[next].send(m);
  });
  await Future.wait<void>(barrier);
}

class Message {
  int start, end;
  bool shouldTranspose = false;
  Float64List data = zeroList;
  SendPort replyTo = mainIsolate.sendPort;
  Message(this.start, this.end);
}

