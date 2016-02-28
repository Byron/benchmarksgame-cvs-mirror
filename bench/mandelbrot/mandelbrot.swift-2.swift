/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/
   contributed by Ralph Ganszky
*/

import Glibc
import Cdispatch
import CBlock

let Iter = 50

// Declare Vec8 as a tuple of 8 double
typealias Vec8 = (Double, Double, Double, Double,
		  Double, Double, Double, Double)

let Zero: Vec8 = (0.0, 0.0, 0.0, 0.0,
		  0.0, 0.0, 0.0, 0.0)

// Declare some basic operators on Vec8
func +(lhs: Vec8, rhs: Double) -> Vec8 {
    var res = lhs
    res.0 += rhs
    res.1 += rhs
    res.2 += rhs
    res.3 += rhs
    res.4 += rhs
    res.5 += rhs
    res.6 += rhs
    res.7 += rhs
    return res
}

func +(lhs: Vec8, rhs: Vec8) -> Vec8 {
    var res = lhs
    res.0 += rhs.0
    res.1 += rhs.1
    res.2 += rhs.2
    res.3 += rhs.3
    res.4 += rhs.4
    res.5 += rhs.5
    res.6 += rhs.6
    res.7 += rhs.7
    return res
}

func -(lhs: Vec8, rhs: Vec8) -> Vec8 {
    var res = lhs
    res.0 -= rhs.0
    res.1 -= rhs.1
    res.2 -= rhs.2
    res.3 -= rhs.3
    res.4 -= rhs.4
    res.5 -= rhs.5
    res.6 -= rhs.6
    res.7 -= rhs.7
    return res
}

func *(lhs: Vec8, rhs: Vec8) -> Vec8 {
    var res = lhs
    res.0 *= rhs.0
    res.1 *= rhs.1
    res.2 *= rhs.2
    res.3 *= rhs.3
    res.4 *= rhs.4
    res.5 *= rhs.5
    res.6 *= rhs.6
    res.7 *= rhs.7
    return res
}

func *(lhs: Double, rhs: Vec8) -> Vec8 {
    var res = rhs
    res.0 *= lhs
    res.1 *= lhs
    res.2 *= lhs
    res.3 *= lhs
    res.4 *= lhs
    res.5 *= lhs
    res.6 *= lhs
    res.7 *= lhs
    return res
}

func *(lhs: Vec8, rhs: Double) -> Vec8 {
    return rhs * lhs
}

func >(lhs: Vec8, rhs: Double) -> Bool {
    if lhs.0 > rhs &&
        lhs.1 > rhs &&
        lhs.2 > rhs &&
        lhs.3 > rhs &&
        lhs.4 > rhs &&
        lhs.5 > rhs &&
        lhs.6 > rhs &&
        lhs.7 > rhs {
            return true
    } else {
        return false
    }
}

// Calculate mandelbrot set for one Vec8 into one byte
func mand8(cr: Vec8, _ ci: Double) -> UInt8 {
    var Zr = Zero
    var Zi = Zero
    var Tr = Zero
    var Ti = Zero
    for _ in 0..<Iter {
        Zi = 2 * Zr * Zi + ci
        Zr = Tr - Ti + cr
        Tr = Zr * Zr
        Ti = Zi * Zi
        if Tr + Ti > 4.0 {
            break
        }
    }
    var byte: UInt8 = 0
    let t = Tr+Ti
    if t.0 <= 4.0 { byte |= 0x80 }
    if t.1 <= 4.0 { byte |= 0x40 }
    if t.2 <= 4.0 { byte |= 0x20 }
    if t.3 <= 4.0 { byte |= 0x10 }
    if t.4 <= 4.0 { byte |= 0x08 }
    if t.5 <= 4.0 { byte |= 0x04 }
    if t.6 <= 4.0 { byte |= 0x02 }
    if t.7 <= 4.0 { byte |= 0x01 }
    return byte
}

// Parse command line arguments
let n: Int
if Process.arguments.count > 1 {
    n = Int(Process.arguments[1]) ?? 200
} else {
    n = 200
}
let N = (n + 7) & ~0x7
let inv = 2.0 / Double(n)
var xvals = [Double](count: N, repeatedValue: 0.0)
var yvals = [Double](count: n, repeatedValue: 0.0)

for i in 0..<N {
    xvals[i] = Double(i) * inv - 1.5
    yvals[i] = Double(i) * inv - 1.0
}

var rows = [UInt8](count: n*N/8, repeatedValue: 0)
let queue = dispatch_get_global_queue(Int(DISPATCH_QUEUE_PRIORITY_DEFAULT), 0)
dispatch_apply(n, queue) { y in
    let ci = yvals[y]
    for x in 0.stride(to: N, by: 8) {
	var cr = Vec8(xvals[x+0], xvals[x+1], xvals[x+2], xvals[x+3],
	    xvals[x+4], xvals[x+5], xvals[x+6], xvals[x+7])
	rows[y*N/8+x/8] = mand8(cr, ci)
    }
}

let header = "P4\n\(n) \(n)\n"
let headerStr = header.withCString({ s in s })

let iov = UnsafeMutablePointer<iovec>.alloc(2)
iov[0].iov_base = UnsafeMutablePointer<Void>(headerStr)
iov[0].iov_len = header.nulTerminatedUTF8.count - 1
rows.withUnsafeMutableBufferPointer{
    (inout p: UnsafeMutableBufferPointer) -> Int in
    iov[1].iov_base = UnsafeMutablePointer<()>(p.baseAddress)
    return 0
}
iov[1].iov_len = rows.count
writev(STDOUT_FILENO, iov, 2)
iov.dealloc(2)
