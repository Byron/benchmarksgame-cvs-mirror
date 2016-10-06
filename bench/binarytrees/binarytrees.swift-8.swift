/* The Computer Language Benchmarks Game
 http://benchmarksgame.alioth.debian.org/
 contributed by David Turnbull
 modified by Pascal Urban (parallel creation of binary trees using libdispatch)
 modified by Maurus Kühne (checkTree uses inout parameters)
 modified for Swift 3.0 by Daniel Muellenborn
 */

import CoreFoundation
import Dispatch

let queue = DispatchQueue.global(qos: .default)

struct TreeNodeItem {
   let left:Int
   let right:Int
   let item:Int
}

func buildTree(_ t: inout Array<TreeNodeItem>, _ item: Int, _ depth: Int) -> Int {
   if depth > 0 {
      t.append(
         TreeNodeItem(
            left: buildTree(&t, 2*item-1, depth-1),
            right:buildTree(&t, 2*item, depth-1),
            item: item
         )
      )
      return t.count - 1
   } else {
      t.append(TreeNodeItem(left: -1, right: -1, item: item))
      return t.count - 1
   }
}

func checkTree(_ t: inout Array<TreeNodeItem>, _ i: Int) -> Int {
   if t[i].left < 0 {
      return 0
   } else {
      return t[i].item + checkTree(&t, t[i].left) - checkTree(&t, t[i].right)
   }
}

let n: Int
if CommandLine.arguments.count > 1 {
   n = Int(CommandLine.arguments[1]) ?? 20
} else {
   n = 20
}

let minDepth = 4
let maxDepth = n
let stretchDepth = n + 1

var longLivedPool = [TreeNodeItem]()

var longLivedTree = buildTree(&longLivedPool, 0, stretchDepth)

let chk = checkTree(&longLivedPool, longLivedTree)
print("stretch tree of depth \(stretchDepth)\t check: \(chk)")

longLivedPool.removeAll(keepingCapacity: true)
longLivedTree = buildTree(&longLivedPool, 0, maxDepth)

let numberOfIterations = (maxDepth - minDepth) / 2 + 1
var checksumOutput = [Int](repeating: 0, count: numberOfIterations)

DispatchQueue.concurrentPerform(iterations: numberOfIterations) { (depthIteration) in
   var shortLivedPool = [TreeNodeItem]()
   let depth = minDepth + depthIteration * 2
   
   let iterations = 1 << (maxDepth - depth + minDepth)
   var check = 0
   for i in 1...iterations {
      shortLivedPool.removeAll(keepingCapacity: true)
      let t1 = buildTree(&shortLivedPool, i, depth)
      check += checkTree(&shortLivedPool, t1)
      shortLivedPool.removeAll(keepingCapacity: true)
      let t2 = buildTree(&shortLivedPool, -i, depth)
      check += checkTree(&shortLivedPool, t2)
   }
   
   checksumOutput[depthIteration] = check
}

for depthIteration in 0..<checksumOutput.count {
   let depth = minDepth + depthIteration * 2
   let iterations = 1 << (maxDepth - depth + minDepth)
   let check = checksumOutput[depthIteration]
   
   print("\(iterations*2)\t trees of depth \(depth)\t check: \(check)")
}

print("long lived tree of depth \(maxDepth)\t check: \(checkTree(&longLivedPool, longLivedTree))")
   
