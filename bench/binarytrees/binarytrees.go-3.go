// The Computer Language Benchmarks Game
// http://benchmarksgame.alioth.debian.org/
//
// Contributed by Alex A Skinner
// Based on the C program by Jeremy Zerfas
// Based on the C++ program from Jon Harrop, Alex Mizrahi, and Bruno Coutinho.

package main

import (
    "fmt"
    "os"
    "runtime"
    "strconv"
    "sync"
)

type nodeArena struct {
    nodes []*node
    index int
}

func (na *nodeArena) get() *node {
    if na.index >= len(na.nodes) {
        na.nodes = append(na.nodes, &node{})
    }
    if na.nodes[na.index] == nil {
        na.nodes[na.index] = &node{}
    }
    na.index++
    return na.nodes[na.index-1]
}

func (na *nodeArena) reset() {
    na.index = 0
}

type node struct {
    value       int
    left, right *node
}

func (n *node) compute() int {
    if n.left != nil {
        return n.left.compute() - n.right.compute() + n.value
    }
    return n.value
}

func createTree(value int, depth int, arena *nodeArena) *node {
    n := arena.get()
    if depth > 0 {
        n.left = createTree(2*value-1, depth-1, arena)
        n.right = createTree(2*value, depth-1, arena)
    } else {
        n.left, n.right = nil, nil
    }
    n.value = value
    return n
}

func main() {
    if len(os.Args) < 2 {
        fmt.Printf("%s <depth>\n", os.Args[0])
        return
    }
    runtime.GOMAXPROCS(runtime.NumCPU() * 2)
    minDepth := 4
    treeDepth, _ := strconv.Atoi(os.Args[1])
    if treeDepth < minDepth+2 {
        treeDepth = minDepth + 2
    }
    na := &nodeArena{}
    stretchTree := createTree(0, treeDepth+1, na)
    fmt.Printf("stretch tree of depth %d\t check: %d\n", treeDepth+1, stretchTree.compute())
    longLived := createTree(0, treeDepth, na)
    toPrint := make([]string, treeDepth+1)
    var wg sync.WaitGroup
    for current := minDepth; current <= treeDepth; current += 2 {
        wg.Add(1)
        go func(depth int) {
            na := &nodeArena{}
            total := 0
            iterations := 1 << uint(treeDepth-depth+minDepth)
            for i := 1; i <= iterations; i++ {
                tree1 := createTree(i, depth, na)
                tree2 := createTree(-i, depth, na)
                total += tree1.compute() + tree2.compute()
                na.reset()
            }
            toPrint[depth] = fmt.Sprintf("%d\t trees of depth %d\t check %d\n", 2*iterations, depth, total)
            wg.Done()
        }(current)
    }
    wg.Wait()
    for current := minDepth; current <= treeDepth; current += 2 {
        fmt.Print(toPrint[current])
    }
    fmt.Printf("long lived tree of depth %d\t check: %d\n", treeDepth, longLived.compute())
}
