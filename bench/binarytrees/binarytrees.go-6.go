/* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * contributed by Edward McFarlane
 */

package main

import (
   "flag"
   "fmt"
   "os"
   "runtime"
   "strconv"
   "sync"
)

type Node struct {
   left, right *Node
   value       int
}

func (root *Node) createTree(pool *sync.Pool, rootNodeValue, treeDepth int) {
   root.value = rootNodeValue

   if treeDepth > 0 {
      if root.left == nil {
         root.left = pool.Get().(*Node)
         root.right = pool.Get().(*Node)
      }

      root.left.createTree(pool, 2*rootNodeValue-1, treeDepth-1)
      root.right.createTree(pool, 2*rootNodeValue, treeDepth-1)

   } else if root.left != nil {
      pool.Put(root.left)
      pool.Put(root.right)
      root.left = nil
      root.right = nil
   }
}

func (root *Node) computeTreeChecksum() int {
   if root.left != nil {
      return root.left.computeTreeChecksum() - root.right.computeTreeChecksum() + root.value
   }
   return root.value
}

func main() {
   flag.Parse()
   n, err := strconv.Atoi(flag.Arg(0))
   if err != nil {
      os.Exit(1)
   }

   minDepth := 4
   maxDepth := n
   if minDepth+2 > n {
      maxDepth = minDepth + 2
   }

   var pool = &sync.Pool{
      New: func() interface{} {
         return &Node{}
      },
   }

   {
      stretchTree := &Node{}
      stretchTree.createTree(pool, 0, maxDepth+1)

      fmt.Printf("stretch tree of depth %d\t check: %d\n", maxDepth+1, stretchTree.computeTreeChecksum())
      pool.Put(stretchTree)
   }

   longLivedTree := pool.Get().(*Node)
   longLivedTree.createTree(pool, 0, maxDepth)

   var wg sync.WaitGroup
   work := make(chan int, 2)
   buf := make([]string, maxDepth+1)
   cpus := runtime.NumCPU()

   wg.Add(cpus)
   for i := 0; i < cpus; i++ {
      go func() {
         defer wg.Done()

         var ok bool
         var depth, iterations, check, i int

         treeRoot := pool.Get().(*Node)
         defer pool.Put(treeRoot)

         for {
            depth, ok = <-work
            if !ok {
               return
            }

            iterations = 1 << uint(maxDepth-depth+minDepth)

            check = 0
            for i = 0; i < iterations; i++ {
               treeRoot.createTree(pool, i, depth)
               check += treeRoot.computeTreeChecksum()
               treeRoot.createTree(pool, -i, depth)
               check += treeRoot.computeTreeChecksum()
            }

            buf[depth] = fmt.Sprintf("%d\t trees of depth %d\t check: %d\n", iterations*2, depth, check)
         }
      }()
   }

   for depth := minDepth; depth <= maxDepth; depth += 2 {
      work <- depth
   }

   close(work)
   wg.Wait()

   for depth := minDepth; depth <= maxDepth; depth += 2 {
      fmt.Print(buf[depth])
   }

   fmt.Printf("long lived tree of depth %d\t check: %d\n", maxDepth, longLivedTree.computeTreeChecksum())
}
