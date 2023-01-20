/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   transliterated from Andrey Filatkin's Node #6 program by Isaac Gouy
*/

import 'dart:isolate';

void main(List<String> args) {
    final maxDepth = (args.length > 0) ? int.parse(args[0]) : 6;

    final stretchDepth = maxDepth + 1;
    final check = itemCheck(bottomUpTree(stretchDepth));
    print("stretch tree of depth $stretchDepth\t check: $check");

    final longLivedTree = bottomUpTree(maxDepth);

    final requests = <Request>[];
    for (var depth = 4; depth <= maxDepth; depth += 2) {
        final iterations = 1 << maxDepth - depth + 4;
        requests.add(Request(iterations,depth));
    }
    var awaitedReplies = requests.length; // each request should receive a reply

    ReceivePort _replyPort = ReceivePort();
    var nIsolates = 4;
    while (nIsolates-- > 0) {
        Isolate.spawn(requestReply,_replyPort.sendPort);
    }

    final replies = <Reply>[]; // will not arrive in order, so cache & sort

    _replyPort.listen((dynamic message) {
        if (message is SendPort) {
            if (requests.length > 0) {
                message.send(requests.removeLast());
            }
        } else {
            replies.add(message as Reply);
            awaitedReplies--;

            if (awaitedReplies < 1){
                replies.sort((a,b) => a.depth.compareTo(b.depth));
                replies.forEach((r) =>
                    print("${r.iterations}\t trees of depth ${r.depth}\t check:
${r.check}")
                );
                print("long lived tree of depth $maxDepth\t check: ${itemCheck(l
ongLivedTree)}");

                _replyPort.close();
            }
        }
    });
}

void requestReply(SendPort p) {
    ReceivePort _requestPort = ReceivePort();
    p.send(_requestPort.sendPort);

    _requestPort.listen((dynamic message) {
        if (message is Request) {
            final iterations = message.iterations;
            final depth = message.depth;
            var check = 0;
            for (int i = 0; i < iterations; i++) {
                check += itemCheck(bottomUpTree(depth));
            }
            p.send(Reply(iterations,depth,check));
            p.send(_requestPort.sendPort);
        }
    });
}

class Request {
    int iterations = 0, depth = 0;
    Request(this.iterations,this.depth);
}

class Reply {
    int iterations = 0, depth = 0, check = 0;
    Reply(this.iterations,this.depth,this.check);
}

class TreeNode {
    TreeNode? left, right;
    TreeNode(this.left,this.right);
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

