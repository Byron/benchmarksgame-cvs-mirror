/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by Alex Bustin
*/


struct TreeNode {
   let item:Int
   let left,right:UnsafeMutablePointer<TreeNode>
   
   func check() -> Int {
      if left != nil {
         return item + left.pointee.check() - right.pointee.check()
      } else {
         return item
      }
   }
}


func bottomUpTree(item:Int, _ depth: Int, _ node:UnsafeMutablePointer<TreeNode>, _ pool:OpaquePointer) {
   
   if depth > 0 {
      let i = item*2
      let d = depth-1
      let l = UnsafeMutablePointer<TreeNode>.init(apr_palloc(pool,sizeof(TreeNode)))
      let r = UnsafeMutablePointer<TreeNode>.init(apr_palloc(pool,sizeof(TreeNode)))
      node.pointee = TreeNode(item:item, left:l, right:r)
      bottomUpTree(i-1, d, l, pool)
      bottomUpTree(i, d, r, pool)
   } else {
      node.pointee = TreeNode(item:item, left:nil, right:nil)
   }
   
}

let n: Int = Int(CommandLine.arguments[1])!
let minDepth = 4
let maxDepth = n
let stretchDepth = n + 1

apr_initialize()
var memoryPool = OpaquePointer()
apr_pool_create_unmanaged_ex(&memoryPool,nil,nil)
     
var pool1 = OpaquePointer()
apr_pool_create_unmanaged_ex(&pool1,nil,nil)

var pool2 = OpaquePointer()
apr_pool_create_unmanaged_ex(&pool2,nil,nil)

var root = UnsafeMutablePointer<TreeNode>.init(apr_palloc(pool1,sizeof(TreeNode)))
bottomUpTree(0,stretchDepth, root, pool1)
let check = root.pointee.check()
print("stretch tree of depth \(stretchDepth)\t check: \(check)")
 
root = UnsafeMutablePointer<TreeNode>.init(apr_palloc(pool2,sizeof(TreeNode)))
bottomUpTree(0,maxDepth,root,pool2)
var depth = minDepth

while depth <= maxDepth {
   let iterations = 1 << (maxDepth - depth + minDepth)
   var check = 0
   for i in 0..<iterations {
      apr_pool_clear(pool1)
      var l = UnsafeMutablePointer<TreeNode>.init(apr_palloc(pool1,sizeof(TreeNode)))
      bottomUpTree(i,depth,l,pool1)
      check += l.pointee.check();
      
      apr_pool_clear(pool1)
      l = UnsafeMutablePointer<TreeNode>.init(apr_palloc(pool1,sizeof(TreeNode)))
      bottomUpTree(-i,depth,l,pool1)
      check += l.pointee.check();
   
   }
   print("\(iterations*2)\t trees of depth \(depth)\t check: \(check)")
   depth += 2
}

print("long lived tree of depth \(maxDepth)\t check: \(root.pointee.check())")
apr_pool_destroy(pool2)
apr_pool_destroy(pool1)
apr_terminate()
