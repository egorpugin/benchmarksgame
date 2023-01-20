/* The Computer Language Benchmarks Game
 * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 *
 * contributed by Chris Boesch
 * based on binary-trees Go #5 program by The Go Authors
 */

package main

import (
    "fmt"
    "os"
    "runtime"
    "strconv"
    "sync"
)

var numCPU int

type Node struct {
    left, right *Node
}

func bottomUpTree(depth int) *Node {
    switch {
    case depth <= 0:
        return &Node{}
    case depth < numCPU:
        return &Node{bottomUpTree(depth - 1), bottomUpTree(depth - 1)}
    default:
        chl := make(chan *Node)
        go func() {
            chl <- bottomUpTree(depth - 1)
        }()
        chr := make(chan *Node)
        go func() {
            chr <- bottomUpTree(depth - 1)
        }()
        return &Node{<-chl, <-chr}
    }
}

func (n *Node) itemCheck() int {
    if n.left == nil {
        return 1
    }
    return 1 + n.left.itemCheck() + n.right.itemCheck()
}

func main() {
    numCPU = runtime.NumCPU()

    minDepth := 4
    maxDepth := 6

    // get argument as maxDepth if greater than initialized
    if len(os.Args) > 1 {
        val, err := strconv.Atoi(os.Args[1])
        if err == nil && val > maxDepth {
            maxDepth = val
        }
    }

    // create strech memory
    stretchDepth := maxDepth + 1
    check := bottomUpTree(stretchDepth).itemCheck()
    fmt.Printf("stretch tree of depth %d\t check: %d\n", stretchDepth, check)

    // create long lived tree
    longLivedTree := bottomUpTree(maxDepth)

    // create all trees in parallel Go routines
    wg := &sync.WaitGroup{}
    res := make([]string, (maxDepth-minDepth)/2+1)
    for depth := minDepth; depth <= maxDepth; depth += 2 {
        depth := depth // create new intance for goroutine
        iterations := 1 << uint(maxDepth-depth+minDepth)
        wg.Add(1)
        go func() {
            defer wg.Done()
            sum := 0
            for i := 0; i < iterations; i++ {
                sum += bottomUpTree(depth).itemCheck()
            }
            res[(depth-minDepth)/2] = fmt.Sprintf(
                "%d\t trees of depth %d\t check: %d", iterations, depth, sum)
        }()
    }
    wg.Wait()

    for _, v := range res {
        fmt.Println(v)
    }

    fmt.Printf("long lived tree of depth %d\t check: %d\n", maxDepth, longLivedT
ree.itemCheck())

}

