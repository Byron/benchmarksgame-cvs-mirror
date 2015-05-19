/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/
   contributed by Isaac Gouy
   parallel by the Anh Team
   Scala Futures version by Robert Wilton
*/
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

object spectralnorm extends App {
  val n = if (args.length > 0) args(0).toInt else 100
  val u, v, tmp = Array.fill(n)(1.0)
  
  // Ordinary and transposed versions of infinite matrix
  val A = (i: Int, j: Int) => 1.0/((i + j) * (i + j + 1) / 2 + i + 1)
  val multiplyAv = multiply(A) _
  val At = (j: Int, i: Int) => 1.0/((i + j) * (i + j + 1) / 2 + i + 1)
  val multiplyAtv = multiply(At) _
  
  // Calculate the chunks and perform calculation.
  val threads = Runtime.getRuntime.availableProcessors
  val chunkSize = 1 + n/threads
  def chunkStart(t: Int) = t * chunkSize
  def chunkEnd(t: Int) = ((t + 1) * chunkSize) min n
  val chunks = (0 until threads) map { t => (chunkStart(t), chunkEnd(t)) }
  printf("%.09f\n",  work(chunks))
  
  // Matrix multiplication for a given range: w <- M*v
  def multiply(M: (Int,Int) => Double)
              (v: Array[Double], w: Array[Double])(start: Int, end: Int) {
    var i = start
    while (i < end) {
     var s = 0.0
     var j = 0
     while (j < n) { s += M(i,j)*v(j); j += 1 }
     w(i) =  s
     i += 1
    }
  }
  
  def work(chunks: Seq[(Int, Int)]) = {
    def split(f: (Int, Int) => Unit) = {
      val res = Future.sequence(
        chunks map { case (start, end) => Future { f(start, end) } } 
      )
      Await.result(res, 1.minute)
    }
    
    for (_ <- 1 to 10) {
      // Multiply by matrix & transpose
      split(multiplyAv(u, tmp))
      split(multiplyAtv(tmp, v))
      split(multiplyAv(v, tmp))
      split(multiplyAtv(tmp, u))
    }

    var vbv,vv = 0.0
    var i = 0
    while (i < n) {
      vbv += u(i)*v(i)
      vv += v(i)*v(i)
      i += 1
    }
    math.sqrt(vbv/vv)
  }
}