/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/
   contributed by Ralph Ganszky
*/

import Glibc
import Cdispatch
import CBlock

// Get number of elements to permute
let n: Int
if Process.arguments.count > 1 {
    n = Int(Process.arguments[1]) ?? 12
} else {
    n = 12
}

// Calculate factorials
var fact = UnsafeMutablePointer<Int>.alloc(n+1)
fact[0] = 1
for i in 1...n {
    fact[i] = fact[i-1] * i
}

// Determine number of tasks
let nchunks = 150
let chunksz = (fact[n] + nchunks - 1) / nchunks
let ntasks = (fact[n] + chunksz - 1) / chunksz

// Allocate result vectors
var maxFlips = [Int](count: ntasks, repeatedValue: 0)
var chkSums = [Int](count: ntasks, repeatedValue: 0)

struct Fannkuch {
    let n: Int
    var p: UnsafeMutablePointer<Int>
    var pp: UnsafeMutablePointer<Int>
    var count: UnsafeMutablePointer<Int>
    
    init(n: Int) {
        self.n = n
        // Allocate buffers of this instance
        p = UnsafeMutablePointer<Int>.alloc(n)
        pp = UnsafeMutablePointer<Int>.alloc(n)
        count = UnsafeMutablePointer<Int>.alloc(n)
    }
    
    mutating func firstPermutation(idx: Int) {
        for i in 0..<n {
            p[i] = i
        }
        
        let start = n - 1
        var indx = idx
        for i in start.stride(to: 0, by: -1) {
            let d = indx / fact[i]
            count[i] = d
            indx = indx % fact[i]
            
            pp.assignFrom(p, count: i+1)
            for j in 0...i {
                p[j] = j+d <= i ? pp[j+d] : pp[j+d-i-1]
            }
        }
    }
    
    mutating func nextPermutation() -> Bool {
        var first = p[1]
        p[1] = p[0]
        p[0] = first
        
        var i = 1
        count[i] += 1
        while count[i] > i {
            count[i] = 0
            i += 1
            p[0] = p[1]
            let next = p[0]
            for j in 1..<i {
                p[j] = p[j+1]
            }
            p[i] = first
            first = next
            count[i] += 1
        }
        return true
    }
    
    mutating func countFlips() -> Int {
        var flips = 1
        var first = p[0]
        if p[first] != 0 {
            pp.assignFrom(p, count: n)
            repeat {
                flips += 1
                var lo = 1
                var hi = first - 1
                while lo < hi {
                    let t = pp[lo]
                    pp[lo] = pp[hi]
                    pp[hi] = t
                    lo += 1
                    hi -= 1
                }
                let t = pp[first]
                pp[first] = first
                first = t
            } while pp[first] != 0
        }
        return flips
    }
    
    mutating func runTask(task: Int) {
        let idxMin = task * chunksz
        let idxMax = (fact[n] < idxMin + chunksz) ? fact[n] : idxMin + chunksz
        
        // Determine first permutation
        firstPermutation(idxMin)
        
        var maxflips = 1
        var chksum = 0
        var i = idxMin
        while true {
            if p[0] != 0 {
                let flips = countFlips()
                if flips > maxflips {
                    maxflips = flips
                }
                chksum += i%2 == 0 ? flips : -flips
            }
            i += 1
            if i == idxMax {
                break
            }
            nextPermutation()
        }
        maxFlips[task] = maxflips
        chkSums[task] = chksum
        p.dealloc(n)
        pp.dealloc(n)
        count.dealloc(n)
        
    }
}

func printResult(n: Int, res: Int, chk: Int) {
    print("\(chk)\nPfannkuchen(\(n)) = \(res)")
}

let group = dispatch_group_create()
let queue = dispatch_get_global_queue(Int(DISPATCH_QUEUE_PRIORITY_DEFAULT), 0)
for i in 0..<ntasks {
    dispatch_group_async(group, queue) {
        var fannkuch = Fannkuch(n: n)
        fannkuch.runTask(i)
    }
}
dispatch_group_wait(group, DISPATCH_TIME_FOREVER)

// Collect results
var res = 0
for flips in maxFlips {
    if flips > res {
        res = flips
    }
}
var chksum = 0
for chk in chkSums {
    chksum += chk
}

printResult(n, res: res, chk: chksum)

fact.dealloc(n+1)

