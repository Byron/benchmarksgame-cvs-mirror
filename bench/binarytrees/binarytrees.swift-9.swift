// The Computer Language Benchmark Game
// http://benchmarksgame.alioth.debian.org/
//
// contributed by Ralph Ganszky

import Dispatch
import Foundation
import apr

let nullptr = OpaquePointer(bitPattern: UInt.max)!

struct Node {
    var left: OpaquePointer
    var right: OpaquePointer
    var value: Int

    func check() -> Int {
	if left != nullptr {
	    let l = UnsafeMutablePointer<Node>(left)
	    let r = UnsafeMutablePointer<Node>(right)
	    return l[0].check() - r[0].check() + value
	} else {
	    return value
	}
    }
}

func CreateTree(_ value: Int, _ depth: Int, _ pool: OpaquePointer) -> OpaquePointer {
    let nodePtr = OpaquePointer(apr_palloc(pool, MemoryLayout<Node>.stride))!
    let node = UnsafeMutablePointer<Node>(nodePtr)
    if depth > 0 {
	node[0].left = CreateTree(2*value-1, depth-1, pool)
	node[0].right = CreateTree(2*value, depth-1, pool)
    } else {
	node[0].left = nullptr
	node[0].right = nullptr
    }
    node[0].value = value
    return nodePtr
}

let minDepth = 4
let maxDepth: Int
if CommandLine.argc > 1 {
    let arg = Int(CommandLine.arguments[1]) ?? 10
    maxDepth = (arg > minDepth + 2) ? (arg & ~(1)) : minDepth + 2
} else {
    maxDepth = 10
}

guard apr_pool_initialize() == APR_SUCCESS else {
    print("Can't initialize apr_pool")
    exit(1)
}

var pool: OpaquePointer? = nil
guard apr_pool_create_unmanaged_ex(&pool, nil, nil) == APR_SUCCESS  else {
    apr_pool_terminate()
    print("Can't create unmanaged pool")
    exit(1)
}

// Create big tree in first pool
let treePtr = CreateTree(0, maxDepth+1, pool!)
let tree = UnsafeMutablePointer<Node>(treePtr) 
print("stretch tree of depth \(maxDepth+1)\t check: \(tree[0].check())")

// Release big tree
apr_pool_clear(pool!);

// Create long living tree
let longLivingTreePtr = CreateTree(0, maxDepth, pool!)

// Allocate trees of increasing depth up to maxDepth depth
let depths = (maxDepth-minDepth)/2+1
var results = [String](repeating: "", count: depths)
let rQueue = DispatchQueue(label: "Result", attributes: [])
let queue = DispatchQueue(label: "Worker", attributes: .concurrent)
DispatchQueue.concurrentPerform(iterations: depths) {
    idx in

    // Create depth pool
    var depthPool: OpaquePointer? = nil
    guard apr_pool_create_unmanaged_ex(&depthPool, nil, nil) == APR_SUCCESS  else {
	apr_pool_terminate()
	print("Can't create unmanaged depth pool")
	exit(1)
    }

    let currentDepth = minDepth + idx * 2
    let iterations = 1 << (maxDepth - currentDepth + minDepth)
    var totalCheckSum = 0
    for i in 1...iterations {
	let tree1Ptr = CreateTree(i, currentDepth, depthPool!)
	let tree2Ptr = CreateTree(-i, currentDepth, depthPool!)
	let tree1 = UnsafeMutablePointer<Node>(tree1Ptr)
	let tree2 = UnsafeMutablePointer<Node>(tree2Ptr)
	totalCheckSum += tree1[0].check() + tree2[0].check()
	apr_pool_clear(depthPool!);
    }

    // Release depth pool
    apr_pool_destroy(depthPool!)

    // Store result string
    rQueue.async {
	results[idx] = "\(2*iterations)\t trees of depth \(currentDepth)\t check: \(totalCheckSum)"
    }
}

// Print output in correnct order
rQueue.sync {
    for result in results {
	print(result)
    }
}

let longLivingTree = UnsafeMutablePointer<Node>(longLivingTreePtr)
print("long lived tree of depth \(maxDepth)\t check: \(longLivingTree[0].check())")

// Release long living tree
apr_pool_destroy(pool!);

apr_pool_terminate()

