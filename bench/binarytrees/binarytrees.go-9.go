/* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * contributed by Ugorji Nwoke
 */

package main

import (
   "fmt"
   "os"
   "strconv"
)

const minDepth = 4

type treeNode struct {
   left, right *treeNode
   item        int
}

func (n *treeNode) itemCheck() int {
   if n.left == nil {
      return n.item
   }
   return n.item + n.left.itemCheck() - n.right.itemCheck()
}

func bottomUp(item, depth int) *treeNode {
   if depth > 0 {
      return &treeNode{
         bottomUp(2*item-1, depth-1),
         bottomUp(2*item, depth-1),
         item,
      }
   }
   return &treeNode{nil, nil, item}
}

func main() {
   n := 0
   if len(os.Args) > 1 {
      if n2, err2 := strconv.ParseInt(os.Args[1], 10, 0); err2 == nil {
         n = int(n2)
      }
   }
   maxDepth := n
   if minDepth+2 > n {
      maxDepth = minDepth + 2
   }
   stretchDepth := maxDepth + 1
   check := bottomUp(0, stretchDepth).itemCheck()
   fmt.Printf("stretch tree of depth %v\t check: %v\n", stretchDepth, check)
   longLivedTree := bottomUp(0, maxDepth)
   for depth := minDepth; depth <= maxDepth; depth += 2 {
      interactions := 1 << uint(maxDepth-depth+minDepth)
      check = 0
      for i := 1; i <= interactions; i++ {
         check += bottomUp(i, depth).itemCheck()
         check += bottomUp(-i, depth).itemCheck()
      }
      fmt.Printf("%v\t trees of depth %v\t check: %v\n", (interactions * 2), depth, check)
   }
   fmt.Printf("long lived tree of depth %v\t check: %v\n", maxDepth, longLivedTree.itemCheck())
}
