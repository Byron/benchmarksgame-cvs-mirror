/* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * contributed by Anthony Perez-Sanz.
 * based on Java program by Jarkko Miettinen
 */

package main

import (
   "flag"
   "fmt"
   "strconv"
)

type Node struct {
   item        int
   left, right *Node
}

const minDepth = 4

func trees(maxDepth int) {
   longLastingNode := createTree(0, maxDepth)
   depth := 4

   for depth <= maxDepth {
      iterations := 1 << uint(maxDepth-depth+minDepth) // 16 << (maxDepth - depth)

      loops(iterations, depth)
      depth += 2
   }
   fmt.Printf("long lived tree of depth %d\t check: %d\n", maxDepth,
      checkTree(longLastingNode))
}

func loops(iterations, depth int) {
   check := 0
   item := 0
   for item < iterations {
      check += checkTree(createTree(item, depth)) +
         checkTree(createTree(-item, depth))
      item++
   }
   fmt.Printf("%d\t trees of depth %d\t check: %d\n",
      iterations<<1, depth, check)
}

func checkTree(n *Node) int {
   if n.left == nil {
      return n.item
   }
   return checkTree(n.left) - checkTree(n.right) + n.item
}

func createTree(item, depth int) *Node {
   node := &Node{item: item}
   if depth > 0 {
      item += item
      depth--
      node.left = createTree(item-1, depth)
      node.right = createTree(item, depth)
   }
   return node
}

func main() {
   n := 0
   flag.Parse()
   if flag.NArg() > 0 {
      n, _ = strconv.Atoi(flag.Arg(0))
   }

   maxDepth := n
   if minDepth+2 > n {
      maxDepth = minDepth + 2
   }

   {
      stretchDepth := maxDepth + 1
      check := checkTree(createTree(0, stretchDepth))
      fmt.Printf("stretch tree of depth %d\t check: %d\n", stretchDepth, check)
   }
   trees(maxDepth)
}
