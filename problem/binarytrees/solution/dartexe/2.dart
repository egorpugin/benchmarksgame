/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   contributed by Isaac Gouy, borrowing from Andrey Filatkinʼs program
*/

import ʼdart:asyncʼ;
import ʼdart:ioʼ;
import ʼdart:isolateʼ;

class TreeNode {
  TreeNode? left, right;
  TreeNode(this.left, this.right);
}

int itemCheck(TreeNode? node) {
  if (node == null || node.left == null) {
    return 1;
  } else {
    return 1 + itemCheck(node.left) + itemCheck(node.right);
  }
}

TreeNode bottomUpTree(int depth) {
  return depth > 0
      ? TreeNode(bottomUpTree(depth - 1), bottomUpTree(depth - 1))
      : TreeNode(null, null);
}

final ports = <SendPort>[];
final requests = <Message>[];
ReceivePort mainIsolate = ReceivePort();

void main(List<String> args) async {
  final n = (args.length > 0) ? int.parse(args[0]) : 6;
  await spawnIsolates();

  final stretchDepth = n + 1;
  final check = itemCheck(bottomUpTree(stretchDepth));
  print("stretch tree of depth $stretchDepth\t check: $check");
  final longLivedTree = bottomUpTree(n);

  setRequests(n);
  var awaitedReplies = requests.length; // each should receive a reply
  final replies = <Message>[];

  mainIsolate.listen((dynamic m) {
    if (m is Message) {
      // accumulate partial results to be sorted
      replies.add(m);
      if (requests.length > 0) {
        // send another request to the isolate that finished
        m.replyTo.send(requests.removeLast());
      }
      if (--awaitedReplies < 1) {
        replies.sort((a, b) => a.depth.compareTo(b.depth));
        replies.forEach((r) => print(
            "${r.iterations}\t trees of depth ${r.depth}\t check: ${r.check}"));
        print(
            "long lived tree of depth $n\t check: ${itemCheck(longLivedTree)}");

        mainIsolate.close();
      }
    }
  });
  // send an initial request to each spawned isolate
  ports.forEach((p) {
    if (requests.length > 0) {
      p.send(requests.removeLast());
    }
  });
}

void setRequests(int n) {
  for (var depth = 4; depth <= n; depth += 2) {
    final iterations = 1 << n - depth + 4;
    requests.add(Message(iterations, depth));
  }
}

Future<void> spawnIsolates() async {
  ReceivePort replyPort = ReceivePort();
  final completers = <Completer>[];
  final barrier = <Future>[];
  var i = Platform.numberOfProcessors;
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
    // update check and send the same message back
    if (m is Message) {
      p = m.replyTo;
      m.check = otherTree(m);
      m.replyTo = requestPort.sendPort;
      p.send(m);
    }
  });
}

int otherTree(Message m) {
  var check = 0;
  for (int i = 0; i < m.iterations; i++) {
    check += itemCheck(bottomUpTree(m.depth));
  }
  return check;
}

class Message {
  int iterations = 0, depth = 0, check = 0;
  SendPort replyTo = mainIsolate.sendPort;
  Message(this.iterations, this.depth);
}


