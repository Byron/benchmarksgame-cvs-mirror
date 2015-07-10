/* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * based on Go program by The Go Authors
 * based on C program by Kevin Carson
 * based on Java program by Heikki Salokanto
 * flag.Arg hack by Isaac Gouy
 * modified by Jamil Djadala to use goroutines
 * modified by Chai Shushan
 * modified by Ainar Garipov to allocate all nodes
 */
package main

import (
    "flag"
    "fmt"
    "runtime"
    "strconv"
    "sync"
)

var minDepth = 4
var n = 0

func main() {
    runtime.GOMAXPROCS(runtime.NumCPU() * 2)

    flag.Parse()
    if flag.NArg() > 0 {
        n, _ = strconv.Atoi(flag.Arg(0))
    }

    maxDepth := n
    if minDepth+2 > n {
        maxDepth = minDepth + 2
    }
    stretchDepth := maxDepth + 1

    stretchCheck := createTree(0, stretchDepth).ItemCheck()
    fmt.Printf("stretch tree of depth %d\t check: %d\n",
        stretchDepth, stretchCheck)

    longLivedTree := createTree(0, maxDepth)

    resultTrees := make([]int, maxDepth+1)
    resultCheck := make([]int, maxDepth+1)

    var wg sync.WaitGroup
    for d := minDepth; d <= maxDepth; d += 2 {
        wg.Add(1)
        go func(depth int) {
            iterations := 1 << uint(maxDepth-depth+minDepth)
            check := 0

            for i := 1; i <= iterations; i++ {
                check += createTree(i, depth).ItemCheck()
                check += createTree(-i, depth).ItemCheck()
            }
            resultTrees[depth] = iterations * 2
            resultCheck[depth] = check

            wg.Done()
        }(d)
    }
    wg.Wait()

    for depth := minDepth; depth <= maxDepth; depth += 2 {
        fmt.Printf("%d\t trees of depth %d\t check: %d\n",
            resultTrees[depth], depth, resultCheck[depth])
    }
    fmt.Printf("long lived tree of depth %d\t check: %d\n",
        maxDepth, longLivedTree.ItemCheck())
}

func createTree(item, depth int) *Node {
    target := 1 << (uint(depth) - 1)
    head, nodes := 0, 0
    queue := make([]Node, target*2)

    queue[head] = Node{item, nil, nil}

    for nodes <= target {
        item *= 2
        n := &queue[head]
        head++

        nodes++
        queue[nodes].item = item - 1
        n.left = &queue[nodes]
        nodes++
        queue[nodes].item = item
        n.right = &queue[nodes]
    }

    return &queue[0]
}

type Node struct {
    item        int
    left, right *Node
}

func (n *Node) ItemCheck() int {
    if n.left == nil {
        return n.item
    }
    return n.item + n.left.ItemCheck() - n.right.ItemCheck()
}
