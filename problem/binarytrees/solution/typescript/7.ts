/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   contributed by Léo Sarrazin
   multi thread by Andrey Filatkin
   sequential by Isaac Gouy
*/

function mainThread(): void {
    const maxDepth = Math.max(6, parseInt(process.argv[2]));

    const stretchDepth = maxDepth + 1;
    const check = itemCheck(bottomUpTree(stretchDepth));
    console.log(`stretch tree of depth ${stretchDepth}\t check: ${check}`);

    const longLivedTree = bottomUpTree(maxDepth);

    for (let depth = 4; depth <= maxDepth; depth += 2) {
        const iterations = 1 << maxDepth - depth + 4;
        work(iterations, depth);
    }

    console.log(`long lived tree of depth ${maxDepth}\t check: ${itemCheck(longL
ivedTree)}`);
}

interface TreeNode {
    left?: TreeNode,
    right?: TreeNode
}

function cTreeNode(left?: TreeNode, right?: TreeNode): TreeNode {
    return {left, right};
}

function work(iterations: number, depth: number): void {
    let check = 0;
    for (let i = 0; i < iterations; i++) {
        check += itemCheck(bottomUpTree(depth));
    }
    console.log(`${iterations}\t trees of depth ${depth}\t check: ${check}`);
}

function itemCheck(node: TreeNode): number {
    if (node.left === undefined || node.right === undefined) {
        return 1;
    }
    return 1 + itemCheck(node.left) + itemCheck(node.right);
}

function bottomUpTree(depth: number): TreeNode {
    return depth > 0
        ? cTreeNode(bottomUpTree(depth - 1), bottomUpTree(depth - 1))
        : cTreeNode(undefined, undefined);
}

mainThread();

