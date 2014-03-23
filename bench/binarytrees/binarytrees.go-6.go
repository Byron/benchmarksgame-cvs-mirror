/* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * based on Go program by The Go Authors.
 * based on C program by Kevin Carson
 * flag.Arg hack by Isaac Gouy
 * modified by Jamil Djadala to use goroutines
 * modified by Chai Shushan
 * modified by Ben Echols to better use goroutines
 */

package main

import (
   "flag"
   "fmt"
   "runtime"
   "strconv"
)

var minDepth = 4
var n = 18

func main() {
   runtime.GOMAXPROCS(runtime.NumCPU())

   flag.Parse()
   if flag.NArg() > 0 {
      n, _ = strconv.Atoi(flag.Arg(0))
   }
   
    maxDepth := n
   if minDepth+2 > n {
      maxDepth = minDepth + 2
   }
   stretchDepth := maxDepth + 1

   check_l := bottomUpTree(0, stretchDepth).ItemCheck()
   fmt.Printf("stretch tree of depth %d\t check: %d\n", stretchDepth, check_l)

   longLivedTree := bottomUpTree(0, maxDepth)

   result_trees := make([]int, maxDepth+1)
   result_check := make([]int, maxDepth+1)
   result_chan := make(chan Result)
   vals := make([]int, (maxDepth - minDepth) / 2 + 1)
   ind := 0
   for depth_l := minDepth; depth_l <= maxDepth; depth_l += 2 {
      vals[ind] = depth_l
      ind += 1
   }
   //sub_elements := int(len(vals) / runtime.NumCPU())
   //if sub_elements == 0 {
   //   sub_elements = 1   
   //}
   for i := 0; i < len(vals); i += 1 {
      //end_slice := i
      //if end_slice > len(vals) {
      //   end_slice = len(vals)
      //}
      go func(depths int, result_chan chan Result) {
         //for j := 0; j < len(depths); j++ {
         makeTree(depths, maxDepth, result_chan)
         //}
      }(vals[i], result_chan)
   }

   for v := minDepth; v <= maxDepth; v += 2 {
      result := <-result_chan
      result_check[result.depth] = result.val
      result_trees[result.depth] = result.iter
   }

   for depth := minDepth; depth <= maxDepth; depth += 2 {
      fmt.Printf("%d\t trees of depth %d\t check: %d\n",
         result_trees[depth], depth, result_check[depth],
      )
   }
   fmt.Printf("long lived tree of depth %d\t check: %d\n",
      maxDepth, longLivedTree.ItemCheck(),
   )
}

func makeTree(depth int, maxDepth int, results chan Result) {
   iterations := 1 << uint(maxDepth-depth+minDepth)
   check := 0
   for i := 1; i <= iterations; i++ {
      check += bottomUpTree(i, depth).ItemCheck()
      check += bottomUpTree(-i, depth).ItemCheck()
   }
   results <- Result{iter: iterations * 2, val: check, depth: depth}
}

func bottomUpTree(item, depth int) *Node {

   if depth <= 0 {
      return &Node{item, nil, nil}
   }
   nodes := make([]Node, (2 << uint(depth)) - 1)
   nodes[0].item = item
   nodes[0].left = &nodes[1]
   nodes[0].right = &nodes[2]
   for i := 1; i < len(nodes); i++ {
      nodes[i].item = nodes[int((i-1)/2)].item * 2
      if i % 2 == 1 {
         nodes[i].item -= 1
      }
      if i < int(len(nodes)/2) {
         nodes[i].left = &nodes[i*2 + 1]
         nodes[i].right = &nodes[i*2 + 2]
      }
   }
   return &nodes[0]
}

type Result struct {
   val   int
   iter  int
   depth int
}

type Node struct {
   item        int
   left, right *Node
}

func (self *Node) ItemCheck() int {
   if self.left == nil {
      return self.item
   }
   return self.item + self.left.ItemCheck() - self.right.ItemCheck()
}
