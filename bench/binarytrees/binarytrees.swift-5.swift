/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by Alex Bustin
*/

import Glibc

struct TreeNode {
   let item:Int
   let left,right:UnsafeMutablePointer<TreeNode>
   
   func check() -> Int {
      if left != nil {
         return item + left.memory.check() - right.memory.check()
      } else {
         return item
      }
   }
}

func bottomUpTree(item:Int, _ depth: Int, _ node:UnsafeMutablePointer<TreeNode>, _ pool:COpaquePointer) {
   
   if depth > 0 {
      let i = item*2
      let d = depth-1
      let l = UnsafeMutablePointer<TreeNode>.init(apr_palloc(pool,sizeof(TreeNode)))
      let r = UnsafeMutablePointer<TreeNode>.init(apr_palloc(pool,sizeof(TreeNode)))
      node.memory = TreeNode(item:item, left:l, right:r)
      bottomUpTree(i-1, d, l, pool)
      bottomUpTree(i, d, r, pool)
   } else {
      node.memory = TreeNode(item:item, left:nil, right:nil)
   }
   
}

struct WorkerData {
   var pid: pthread_t
   var check, iterations, depth:Int
}

func worker_thread(tdata:UnsafeMutablePointer<Void>) -> UnsafeMutablePointer<Void> {

   let data = UnsafeMutablePointer<WorkerData>(tdata)
   let iterations = data.memory.iterations
   let depth = data.memory.depth
   var check = 0
     
   var pool = COpaquePointer()
   apr_pool_create_unmanaged_ex(&pool,nil,nil)

   for i in 0..<iterations {
      var l = UnsafeMutablePointer<TreeNode>.init(apr_palloc(pool,sizeof(TreeNode)))
      bottomUpTree(i,depth,l,pool)
      check += l.memory.check();
      
      apr_pool_clear(pool)
      l = UnsafeMutablePointer<TreeNode>.init(apr_palloc(pool,sizeof(TreeNode)))
      bottomUpTree(-i,depth,l,pool)
      check += l.memory.check();
   }
   
   apr_pool_destroy(pool)
   data.memory.check = check
   return nil
}


let n: Int = Int(Process.arguments[1])!
let minDepth = 4
let maxDepth = n
let stretchDepth = n + 1
let size = (maxDepth-minDepth)/2 + 1 
var workers_head = UnsafeMutablePointer<WorkerData>.alloc(size);

apr_initialize()
var pool1 = COpaquePointer()
apr_pool_create_unmanaged_ex(&pool1,nil,nil)

var root = UnsafeMutablePointer<TreeNode>.init(apr_palloc(pool1,sizeof(TreeNode)))
bottomUpTree(0,stretchDepth, root, pool1)
let check = root.memory.check()
print("stretch tree of depth \(stretchDepth)\t check: \(check)")
 
apr_pool_clear(pool1)

root = UnsafeMutablePointer<TreeNode>.init(apr_palloc(pool1,sizeof(TreeNode)))
bottomUpTree(0,maxDepth,root,pool1)

var depth = minDepth
var worker = workers_head

while depth <= maxDepth {

   worker.memory = WorkerData(
      pid:0,
      check:0,
      iterations: 1 << (maxDepth - depth + minDepth),
      depth:depth
   )

   pthread_create(&worker.memory.pid, nil, worker_thread, worker)
   depth += 2
   worker += 1
}


for i in 0..<size {
   let worker = workers_head + i
   pthread_join(worker.memory.pid,nil)
   print("\(worker.memory.iterations*2)\t trees of depth \(worker.memory.depth)\t check: \(worker.memory.check)")
}

print("long lived tree of depth \(maxDepth)\t check: \(root.memory.check())")

apr_pool_destroy(pool1)
apr_terminate()

