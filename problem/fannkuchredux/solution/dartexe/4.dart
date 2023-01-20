/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   transliterated from Andrey Filatkinʼs Node #5 program by Isaac Gouy
*/

import ʼdart:isolateʼ;
import ʼdart:typed_dataʼ;
import ʼdart:mathʼ;
import ʼdart:ioʼ;

Int32List nFactorials(int n) {
  final fact = Int32List(n + 1);
  fact[0] = 1;
  for (int i = 1; i <= n; i++) {
    fact[i] = fact[i - 1] * i;
  }
  return fact;
}

Reply fannkuch(Request r) {
  final n = r.n;
  final Int32List fact = r.fact;

  final p = Int32List(n);
  final pp = Int32List(n);
  final count = Int32List(n);

  // first permutation
  for (var i = 0; i < n; i++) {
    p[i] = i;
  }
  int idx = r.idxMin;
  for (var i = n - 1; i > 0; i--) {
    final d = div(idx, fact[i]);
    count[i] = d;
    idx = idx % fact[i];

    for (var j = 0; j < n; j++) {
      pp[j] = p[j];
    }

    for (var j = 0; j <= i; j++) {
      if (j + d <= i) {
        p[j] = pp[j + d];
      } else {
        p[j] = pp[j + d - i - 1];
      }
    }
  }

  final reply = Reply();

  idx = r.idxMin;
  for (var sign = true;; sign = !sign) {
    var first = p[0];
    if (first != 0) {
      var flips = 1;
      if (p[first] != 0) {
        for (var j = 1; j < n; j++) {
          pp[j] = p[j];
        }
        var p0 = first;
        while (true) {
          flips++;
          var i = 1;
          var j = p0 - 1;
          while (i < j) {
            final t = pp[i];
            pp[i] = pp[j];
            pp[j] = t;
            i++;
            j--;
          }
          final t = pp[p0];
          pp[p0] = p0;
          p0 = t;
          if (pp[p0] == 0) {
            break;
          }
        }
      }

      if (reply.flips < flips) {
        reply.flips = flips;
      }
      if (sign) {
        reply.sum += flips;
      } else {
        reply.sum -= flips;
      }
    }

    idx++;
    if (idx == r.idxMax) {
      break;
    }

    // next permutation
    if (sign) {
      p[0] = p[1];
      p[1] = first;
    } else {
      final t = p[1];
      p[1] = p[2];
      p[2] = t;
      for (var k = 2;; k++) {
        count[k]++;
        if (count[k] <= k) {
          break;
        }
        count[k] = 0;
        for (var j = 0; j <= k; j++) {
          p[j] = p[j + 1];
        }
        p[k + 1] = first;
        first = p[0];
      }
    }
  }
  return reply;
}

int div(int val, int by) {
  return (val - val % by) ~/ by;
}

void main(List<String> args) {
  final n = (args.length > 0) ? int.parse(args[0]) : 7;
  final fact = nFactorials(n);

  const nchunks = 720;
  var chunkSize = div((fact[n] + nchunks - 1), nchunks);
  chunkSize += chunkSize % 2;

  final requests = <Request>[];
  final len = div((fact[n] + chunkSize - 1), chunkSize);
  for (var i = 0; i < len; i++) {
    final idxMin = chunkSize * i;
    final idxMax = min(fact[n], idxMin + chunkSize);
    requests.add(Request(n, idxMin, idxMax, fact));
  }
  var awaitedReplies = requests.length; // each request should receive a reply

  ReceivePort replyPort = ReceivePort();
  var i = Platform.numberOfProcessors;
  while (i-- > 0) {
    Isolate.spawn(requestReply, replyPort.sendPort);
  }

  var checkSum = 0, maxFlips = 0;

  replyPort.listen((dynamic message) {
    if (message is SendPort) {
      if (requests.length > 0) {
        message.send(requests.removeLast());
      }
    } else {
      final reply = message as Reply;
      checkSum += reply.sum;
      if (reply.flips > maxFlips) {
        maxFlips = reply.flips;
      }
      if (--awaitedReplies < 1) {
        print("$checkSum\nPfannkuchen($n) = $maxFlips");
        replyPort.close();
      }
    }
  });
}

void requestReply(SendPort p) {
  ReceivePort requestPort = ReceivePort();
  p.send(requestPort.sendPort);

  requestPort.listen((dynamic message) {
    if (message is Request) {
      p.send(fannkuch(message));
      p.send(requestPort.sendPort);
    }
  });
}

class Request {
  int n = 0, idxMin = 0, idxMax = 0;
  Int32List fact;
  Request(this.n, this.idxMin, this.idxMax, this.fact);
}

class Reply {
  int sum = 0, flips = 0;
}

