/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/
   contributed by Isaac Gouy 
   formatDouble fix by Joao Pedrosa 
*/

import Glibc

func approximate(n: Int) -> Double {
   var u = Array(count: n, repeatedValue: 1.0) 

   var v = Array(count: n, repeatedValue: 0.0) 
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

func a(i: Int, _ j: Int) -> Double {
   let ij = i+j
   return 1.0 / Double( ij*(ij+1)/2 + i+1 ) 
}

func multiplyAv(n: Int, _ v: [Double], inout _ av: [Double]) {
   for i in 0..<n {
      av[i] = 0.0;
      for j in 0..<n {
         av[i] += a(i,j)*v[j] 
      }
   }
}

func multiplyAtv(n: Int, _ v: [Double], inout _ atv: [Double]) {
   for i in 0..<n {
      atv[i] = 0;
      for j in 0..<n {
         atv[i] += a(j,i)*v[j] 
      }
   }
}

func multiplyAtAv(n: Int, _ v: [Double], inout _ atAv: [Double]) {
   var u = Array(count: n, repeatedValue: 0.0) 
   multiplyAv(n,v,&u)
   multiplyAtv(n,u,&atAv)
}


func formatDouble(f: Double, precision: Int = 2) -> String {
   var p = 1
   for _ in 0..<precision {
      p *= 10
   }
   let neutral = abs(Int(round((f * Double(p)))))
   var s = ""
   let a = "\(neutral)".characters
   let len = a.count
   var dot = len - precision
   if f < 0 {
      s += "-"
   }
   if dot <= 0 {
      dot = 1
   }
   let pad = precision - len
   var i = 0
   while i <= pad {
      s += i == dot ? ".0" : "0"
      i += 1
   }
   for c in a {
      if i == dot {
         s += "."
      }
      s.append(c)
      i += 1
   }
   return s
}


let n: Int = Int(Process.arguments[1])!
print("\(formatDouble(approximate(n), precision: 9))")
