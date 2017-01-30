// The Computer Language Benchmark Game
// http://benchmarksgame.alioth.debian.org/
//
// contributed by Ralph Ganszky

import Dispatch
import Foundation

class TreeNode {
    var left, right: TreeNode?
    var value:Int

    init() {
	value = 0
    }

    init(left: TreeNode?, right: TreeNode?, value: Int) {
        self.left = left
	self.right = right
	self.value = value
    }

    func check() -> Int {
	if left != nil {
	    return left!.check() - right!.check() + value
	} else {
	    return value
	}
    }
}

func createTree(_ rootValue:Int, _ depth: Int) -> TreeNode? {
    if depth > 0 {
	let node = TreeNode(left: createTree(2*rootValue-1, depth-1),
			    right: createTree(2*rootValue, depth-1),
			    value: rootValue)
	return node
    } else {
	let node = TreeNode(left: nil, right: nil, value: rootValue)
	return node
    }
}

let n: Int
if CommandLine.argc > 1 {
    n = Int(CommandLine.arguments[1]) ?? 10
} else {
    n = 10
}
let minDepth = 4
let maxDepth = (n > minDepth + 2) ? n : minDepth + 2

// Create big tree in first pool
let tree = createTree(0, maxDepth+1)
let check = tree!.check()
print("stretch tree of depth \(maxDepth+1)\t check: \(check)")

// Cleal first pool and allocate long living tree
let longLivingTree = createTree(0, maxDepth)

// Allocate binary trees of increasing depth up to maxDepth depth
let group = DispatchGroup()
let rq = DispatchQueue(label: "Result", attributes: [])
let queue = DispatchQueue(label: "Worker", attributes: .concurrent)
var results = [String](repeating: "", count: (maxDepth-minDepth)/2+1)
for currentDepth in stride(from: minDepth, through: maxDepth, by: 2) {
    queue.async(group: group) {
        let idx = (currentDepth - minDepth) / 2
	let iterations = 1 << (maxDepth - currentDepth + minDepth)
	var totalChecksum = 0
	for i in 1...iterations {
	    let tree1 = createTree(i, currentDepth)
	    let tree2 = createTree(-i, currentDepth)
	    totalChecksum += tree1!.check() + tree2!.check()
	}
	rq.async{
	    results[idx] = "\(2*iterations)\t trees of depth \(currentDepth)\t check: \(totalChecksum)"
	}
    }
}
group.wait()

rq.sync {
    for result in results {
	print(result)
    }
}

// Check long living tree and print out check info
print("long lived tree of depth \(maxDepth)\t check: \(longLivingTree!.check())")

