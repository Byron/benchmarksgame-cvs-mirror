/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by Kannan Goundan
   modified by Isaac Gouy
   optimized by David Pollak
   updated to 2.8 by Rex Kerr
   parallelized by Yang Bo
*/
 
import scala.concurrent.duration._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global 
object binarytrees {
  def main(args: Array[String]) = {
    val n = try{ args(0).toInt } catch { case _ => 1 }
    val minDepth = 4
    val maxDepth = n max (minDepth+2)
 
    def print(name: String, depth: Int, check: Int) =
      println(name + " of depth " + depth + "\t check: " + check)
 
    print("stretch tree", maxDepth+1, Await.result(Tree(0,maxDepth+1), Duration.Inf).isum)
    val longLivedTree = Await.result(Tree(0,maxDepth), Duration.Inf)
    var depth = minDepth
    while (depth <= maxDepth) {
      val iterations = 1 << (maxDepth - depth + minDepth)
      var i,sum = 0
      while (i < iterations) {
        i += 1
        sum += Await.result(Tree(i,depth), Duration.Inf).isum + Await.result(Tree(-i,depth), Duration.Inf).isum
      }
      print(iterations*2 + "\t trees", depth, sum)
      depth += 2
    }
    print("long lived tree", maxDepth, longLivedTree.isum)
  }
}
 
final class Tree(i: Int, left: Tree, right: Tree) {
  def isum: Int = {
    val tl = left
    if (tl eq null) i
    else i + tl.isum - right.isum
  }
}
object Tree {
  def apply(i: Int, depth: Int, futureDepth: Int = 0): Future[Tree] = {
    if (futureDepth < 4) {
      if (depth > 0) Tree(i*2-1, depth-1, futureDepth+1).zip(Tree(i*2, depth-1, futureDepth+1)).map { case (left, right) => new Tree(i, left, right) }
      else Future(new Tree(i, null, null))
    } else {
      def synchronizedApply(i: Int, depth: Int):Tree = {
        if (depth > 0) new Tree(i, synchronizedApply(i*2-1, depth-1), synchronizedApply(i*2, depth-1))
        else new Tree(i, null, null)
      }
      Future(synchronizedApply(i, depth))
    }
  }
}
