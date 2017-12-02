/* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * porting the C GCC version to Go
 * Contributed by anon
 */

package main

import (
   "fmt"
   "os"
   "strconv"
   "sync"
)

var results map[int]int
var wg sync.WaitGroup
var mtx sync.Mutex

type node struct {
   left  *node
   right *node
}

func (n *node) Checksum() int {
   if n.left == nil {
      return 1
   } else {
      return n.left.Checksum() + n.right.Checksum() + 1
   }
}

func (n *node) Clear() {
   if n.left != nil {
      n.left.Clear()
      n.right.Clear()
   }

   n = nil
}

func NewNode(depth int) *node {
   n := new(node)

   if depth > 0 {
      n.left = NewNode(depth - 1)
      n.right = NewNode(depth - 1)
   }

   return n
}

func UpdateResults(depth, sum int) {
   mtx.Lock()
   results[depth] = sum
   mtx.Unlock()
}

func ManyTrees(iterations, depth int) {
   defer wg.Done()

   sum := 0

   for i := 0; i < iterations; i++ {
      n := NewNode(depth)
      sum += n.Checksum()
      n.Clear()
   }

   UpdateResults(depth, sum)
}

func main() {
   minDepth := 4
   maxDepth := minDepth + 2
   results = make(map[int]int)

   if len(os.Args) == 2 {
      val, err := strconv.Atoi(os.Args[1])
      if err == nil {
         maxDepth = val
      }
   }

   // Stretch tree
   stretch := NewNode(maxDepth + 1)
   fmt.Printf("stretch tree of depth %d\t check: %d\n", maxDepth+1, stretch.Checksum())
   stretch.Clear()

   // Long lived tree for later use
   long := NewNode(maxDepth)

   // Lots of trees in parallel
   for i := minDepth; i <= maxDepth; i += 2 {
      count := 1 << uint(maxDepth-i+minDepth)
      wg.Add(1)
      go ManyTrees(count, i)
   }
   wg.Wait()

   for i := minDepth; i <= maxDepth; i += 2 {
      count := 1 << uint(maxDepth-i+minDepth)
      fmt.Printf("%d\t trees of depth %d\t check: %d\n", count, i, results[i])
   }

   // Long lived tree stats
   fmt.Printf("long lived tree of depth %d\t check: %d\n", maxDepth, long.Checksum())
   stretch.Clear()
}
