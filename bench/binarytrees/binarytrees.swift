/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/
   contributed by Isaac Gouy 
*/


class TreeNode {
   var left, right : TreeNode?
   var item : Int

   init(_ left: TreeNode?, _ right: TreeNode?, _ item: Int) {
      self.left = left
      self.right = right
      self.item = item
   }

   func check() -> Int {
      guard let left = left, let right = right else {
         return item 
      }
      return item + left.check() - right.check()
   }
}

func bottomUpTree(item: Int, _ depth: Int) -> TreeNode {
   if depth > 0 {
      return 
         TreeNode(
            bottomUpTree(2*item-1, depth-1),
            bottomUpTree(2*item, depth-1),
            item
         )
   }
   else {
      return 
         TreeNode(nil,nil,item)
   }
}


let n: Int = Int(Process.arguments[1])!
let minDepth = 4
let maxDepth = n
let stretchDepth = n + 1

let check = bottomUpTree(0,stretchDepth).check()
print("stretch tree of depth \(stretchDepth)\t check: \(check)")

let longLivedTree = bottomUpTree(0,maxDepth)

var depth = minDepth
while depth <= maxDepth {
   let iterations = 1 << (maxDepth - depth + minDepth)
   var check = 0
   for i in 0..<iterations {
      check += bottomUpTree(i,depth).check()
      check += bottomUpTree(-i,depth).check()
   }
   print("\(iterations*2)\t trees of depth \(depth)\t check: \(check)")
   depth += 2
}

print("long lived tree of depth \(maxDepth)\t check: \(longLivedTree.check())")

