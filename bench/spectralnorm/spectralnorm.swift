/* The Computer Language Benchmarks Game
 http://benchmarksgame.alioth.debian.org/
 contributed by Isaac Gouy
 converted to Swift 3 by Sergo Beruashvili
 */

import Glibc
//import Darwin
import Foundation

func approximate(_ n: Int) -> Double {
    var u = Array(repeating: 1.0, count: n)
    
    var v = Array(repeating: 0.0, count: n)
    for _ in 1...10 {
        multiplyAtAv(n,u,&v)
        multiplyAtAv(n,v,&u)
    }
    
    var vBv = 0.0, vv = 0.0
    for i in 0..<n {
        vBv += u[i]*v[i]
        vv  += v[i]*v[i]
    }
    
    return sqrt(vBv/vv)
}

func a(_ i: Int, _ j: Int) -> Double {
    let ij = i+j
    return 1.0 / Double( ij*(ij+1)/2 + i+1 )
}

func multiplyAv(_ n: Int, _ v: [Double], _ av: inout [Double]) {
    for i in 0..<n {
        av[i] = 0.0;
        for j in 0..<n {
            av[i] += a(i,j)*v[j]
        }
    }
}

func multiplyAtv(_ n: Int, _ v: [Double], _ atv: inout [Double]) {
    for i in 0..<n {
        atv[i] = 0;
        for j in 0..<n {
            atv[i] += a(j,i)*v[j]
        }
    }
}

func multiplyAtAv(_ n: Int, _ v: [Double], _ atAv: inout [Double]) {
    var u = Array(repeating: 0.0, count: n)
    multiplyAv(n,v,&u)
    multiplyAtv(n,u,&atAv)
}


let n: Int = Int(CommandLine.arguments[1])!
print( String(format: "%.9f", approximate(n)) )
