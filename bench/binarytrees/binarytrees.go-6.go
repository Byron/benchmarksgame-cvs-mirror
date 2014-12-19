/* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * based on Go program by The Go Authors.
 * based on C program by Kevin Carson
 * flag.Arg hack by Isaac Gouy
 * modified by Jamil Djadala to use goroutines
 * modified by Chai Shushan
 * modified by Chandra Sekar S to use arenas
 * modified by Fabien Delorme to GC less often and to reduce slicing of slices
 */
package main

import (
   "flag"
   "fmt"
   "runtime"
   "runtime/debug"
   "strconv"
   "sync"
)

const minDepth = 4
const arenaSize = 2000

type Node struct {
   item        int
   left, right *Node
}

type NodeArena struct {
   nodes []Node
   size int
}

func (na *NodeArena) Get() *Node {
   if na.size == 0 {
      *na = NodeArena{make([]Node, arenaSize), arenaSize}
   }

   na.size--

   return &(*na).nodes[na.size]
}

func main() {
   runtime.GOMAXPROCS(runtime.NumCPU() * 2)
   // A bigger GCPercent augments memory footprint but reduces execution time
   debug.SetGCPercent(300)
   flag.Parse()

   var maxDepth int

   if flag.NArg() > 0 {
      maxDepth, _ = strconv.Atoi(flag.Arg(0))
   }

   if minDepth+2 > maxDepth {
      maxDepth = minDepth + 2
   }

   stretchDepth := maxDepth + 1
   mainArena := NodeArena{make([]Node, arenaSize), arenaSize}

   check_l := bottomUpTree(0, stretchDepth, &mainArena).ItemCheck()
   fmt.Printf("stretch tree of depth %d\t check: %d\n", stretchDepth, check_l)

   longLivedTree := bottomUpTree(0, maxDepth, &mainArena)

   result_trees := make([]int, maxDepth+1)
   result_check := make([]int, maxDepth+1)

   var wg sync.WaitGroup

   for depth_l := minDepth; depth_l <= maxDepth; depth_l += 2 {
      wg.Add(1)

      go func(depth int) {
         localArena := NodeArena{make([]Node, arenaSize), arenaSize}
         iterations := 1 << uint(maxDepth-depth+minDepth)
         check := 0

         for i := 1; i <= iterations; i++ {
            check += bottomUpTree(i, depth, &localArena).ItemCheck()
            check += bottomUpTree(-i, depth, &localArena).ItemCheck()
         }

         result_trees[depth] = iterations * 2
         result_check[depth] = check

         wg.Done()
      }(depth_l)
   }

   wg.Wait()

   for depth := minDepth; depth <= maxDepth; depth += 2 {
      fmt.Printf("%d\t trees of depth %d\t check: %d\n",
         result_trees[depth], depth, result_check[depth])
   }

   fmt.Printf("long lived tree of depth %d\t check: %d\n",
      maxDepth, longLivedTree.ItemCheck())
}

func bottomUpTree(item, depth int, arena *NodeArena) *Node {
   n := arena.Get()
   n.item = item

   if depth > 0 {
      n.left = bottomUpTree(2 * item - 1, depth - 1, arena)
      n.right = bottomUpTree(2 * item, depth - 1, arena)
   }

   return n
}

func (self *Node) ItemCheck() int {
   if self.left == nil {
      return self.item
   } else {
      return self.item + self.left.ItemCheck() - self.right.ItemCheck()
   }
}
