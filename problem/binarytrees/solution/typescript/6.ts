/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   contributed by Léo Sarrazin
   multi thread by Andrey Filatkin
   converted by Isaac Gouy
*/

import { Worker, isMainThread, parentPort, workerData } from 'worker_threads';

if (isMainThread) {
    mainThread();
} else {
    workerThread(workerData);
}

async function mainThread() {
    const maxDepth = Math.max(6, parseInt(process.argv[2]));

    const stretchDepth = maxDepth + 1;
    const check = itemCheck(bottomUpTree(stretchDepth));
    console.log(`stretch tree of depth ${stretchDepth}\t check: ${check}`);

    const longLivedTree = bottomUpTree(maxDepth);

    const tasks: Data[] = [];
    for (let depth = 4; depth <= maxDepth; depth += 2) {
        const iterations = 1 << maxDepth - depth + 4;
        tasks.push({iterations, depth});
    }

    const results = await runTasks(tasks);
    for (const result of results) {
        console.log(result);
    }

    console.log(`long lived tree of depth ${maxDepth}\t check: ${itemCheck(longL
ivedTree)}`);
}

function workerThread({iterations, depth}: Data) {
    parentPort?.postMessage({
        result: work(iterations, depth)
    });
}

function runTasks(tasks: Data[]): Promise<string[]> {
    return new Promise(resolve => {
        const results: string[] = [];
        let tasksSize = tasks.length;

        for (let i = 0; i < tasks.length; i++) {
            const worker = new Worker(__filename, {workerData: tasks[i]});

            worker.on('message', message => {
                results[i] = message.result;
                tasksSize--;
                if (tasksSize === 0) {
                    resolve(results);
                }
            });
        }
    });
}

function work(iterations: number, depth: number): string {
    let check = 0;
    for (let i = 0; i < iterations; i++) {
        check += itemCheck(bottomUpTree(depth));
    }
    return `${iterations}\t trees of depth ${depth}\t check: ${check}`;
}

function cTreeNode(left?: TreeNode, right?: TreeNode): TreeNode {
    return {left, right};
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

interface Data {
    iterations: number;
    depth: number;
}

interface TreeNode {
    left?: TreeNode,
    right?: TreeNode
}

