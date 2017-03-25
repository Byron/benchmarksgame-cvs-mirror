/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by Kannan Goundan
   modified by Isaac Gouy
   optimized by David Pollak
   updated to 2.8 by Rex Kerr
   parallelized by Yang Bo
   *reset*
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
 
    print("stretch tree", maxDepth+1, Await.result(Tree(maxDepth+1), Duration.Inf).isum)
    val longLivedTree = Await.result(Tree(maxDepth), Duration.Inf)
    var depth = minDepth
    while (depth <= maxDepth) {
      val iterations = 1 << (maxDepth - depth + minDepth)
      var i,sum = 0
      while (i < iterations) {
        i += 1
        sum += Await.result(Tree(depth), Duration.Inf).isum
      }
      print(iterations + "\t trees", depth, sum)
      depth += 2
    }
    print("long lived tree", maxDepth, longLivedTree.isum)
  }
}
 
final class Tree(left: Tree, right: Tree) {
  def isum: Int = {
    val tl = left
    if (tl eq null) 1
    else 1 + tl.isum + right.isum
  }
}
object Tree {
  def apply(depth: Int, futureDepth: Int = 0): Future[Tree] = {
    if (futureDepth < 4) {
      if (depth > 0) 
         Tree(depth-1, futureDepth+1).zip(Tree(depth-1, futureDepth+1)).map { 
            case (left, right) => new Tree(left, right) }
      else 
         Future(new Tree(null, null))
    } else {
      def synchronizedApply(depth: Int):Tree = {
        if (depth > 0) new Tree(synchronizedApply(depth-1), synchronizedApply(depth-1))
        else new Tree(null, null)
      }
      Future(synchronizedApply(depth))
    }
  }
}
