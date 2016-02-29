/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/
   contributed by Ralph Ganszky 
*/

import Glibc
import Cdispatch
import CBlock

// Get matrix dimension
let n: Int
if Process.arguments.count > 1 {
    n = Int(Process.arguments[1]) ?? 100
} else {
    n = 100
}

let queue = dispatch_get_global_queue(Int(DISPATCH_QUEUE_PRIORITY_DEFAULT), 0)

func A(i: Int, _ j: Int) -> Double {
    return 1.0 / Double((i+j)*(i+j+1)/2 + (i+1))
}

func multiplyAv(n: Int, _ v: [Double]) -> [Double] {
    var Av = [Double](count: n, repeatedValue: 0.0)
    dispatch_apply(n, queue) { i in
        var AvTemp = 0.0
        for j in 0..<n {
            AvTemp += A(i,j) * v[j]
        }
	Av[i] = AvTemp      // This is unprotected and could cause
			    // a problem. Maybe here some atomic update
			    // or a semaphore should be used
    }
    return Av
}

func multiplyAtv(n: Int, _ v: [Double]) -> [Double] {
    var Atv = [Double](count: n, repeatedValue: 0.0)
    dispatch_apply(n, queue) { i in
        var AtvTemp = 0.0
        for j in 0..<n {
            AtvTemp += A(j,i) * v[j]
        }
	Atv[i] = AtvTemp
    }
    return Atv
}

func multiplyAtAv(n: Int, _ v: [Double]) -> [Double] {
    let u = multiplyAv(n, v)
    return multiplyAtv(n, u)
}

func approximate(n: Int) -> Double {
    var u = [Double](count: n, repeatedValue: 1.0)
    var v = [Double]()
    for _ in 0..<10 {
        v = multiplyAtAv(n, u)
        u = multiplyAtAv(n, v)
    }
    var vBv = 0.0
    var vv = 0.0
    for i in 0..<n {
        vBv += u[i]*v[i]
        vv += v[i]*v[i]
    }
    return sqrt(vBv/vv)
}

func roundDouble(num: Double, precision: Int) -> String {
    let exponent = pow(10.0, Double(precision))
    let number = Double(Int(num * exponent + 0.5))/exponent
    return "\(number)"
}

print(roundDouble(approximate(n), precision: 9))
