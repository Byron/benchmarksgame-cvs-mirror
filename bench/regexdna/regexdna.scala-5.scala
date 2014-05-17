/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by Rex Kerr
   based partially on Java solution by Jason Nordwick
*/

import java.util.regex._
import java.util.concurrent._
import java.util.Arrays.{copyOfRange => dup}

// We won't be needing subsequences
trait CS extends CharSequence {
  def subSequence(i0: Int, i1: Int) = ???
}

// A byte-array-backed CharSequence (i.e. like a String)
class ByteCS(val buf: Array[Byte])(var length: Int = buf.length) extends CS {
  def charAt(i: Int) = buf(i).toChar
  def ++=(arr: Array[Byte], i0: Int, i1: Int): ByteCS = {
    val ans = { if (length+i1-i0 < buf.length) this else new ByteCS(dup(buf, 0, buf.length*2))(length) }
    var i = i0
    var j = ans.length
    while (i < i1) { ans.buf(j) = arr(i); j += 1; i += 1 }
    ans.length = j
    ans
  }
  override def toString = new String(buf,0,length)
}

// A "string" made from a bunch of byte arrays
class MegaCS(val bcs: Array[ByteCS]) extends CS {
  def charAt(i: Int) = {
    var j = i
    var k = 0
    while (j >= bcs(k).length) { j -= bcs(k).length; k += 1 }
    bcs(k).charAt(j)
  }
  def length = bcs.map(_.length).sum
  override def toString = bcs.map(_.toString).mkString
}

// Represents work to complete
trait Work {
  def run: Seq[Work]
  def pri: Int
}
object Done extends Work {
  def run = Seq(Done)
  def pri = Int.MaxValue
}

object regexdna {
  val cores = java.lang.Runtime.getRuntime.availableProcessors

  // Runs & collects jobs
  def doWork(works: Seq[Work]) {
    val pri = new PriorityBlockingQueue(12, new java.util.Comparator[Work] {
      def compare(w: Work, v: Work) = w.pri compare v.pri
    })
    works.foreach(pri put _)
    val complete = new CountDownLatch(cores)
    for (i <- 0 until cores) {
      val t = new Thread { override def run() {
        var w: Work = null
        while (w ne Done) {
          w = pri.take
          w.run.foreach{ pri.put }
        }
        pri.put(Done)
        complete.countDown
      }}
      t.start
    }
    complete.await()
  }

  def main(args : Array[String]) {
    // load data from stdin
    val init = {
      import java.io._
      import java.nio.ByteBuffer
      val in = (new FileInputStream(FileDescriptor.in)).getChannel
      val bb = ByteBuffer.allocate(in.size.toInt)
      in.read(bb)
      new ByteCS(bb.array)()
    }

    // strip header & newline
    val input = {
      var bcs = new ByteCS(new Array[Byte](init.length))(0)
      val m = Pattern.compile(">.*\n|\n").matcher(init)
      var i = 0
      while (m.find) {
        bcs = (bcs ++= (init.buf, i, m.start))
        i = m.end
      }
      bcs ++= (init.buf, i, init.length)
    }
    val inc = math.max(1,(input.length+15)/16)

    // patterns to count
    val patterns  = Seq(
      "agggtaaa|tttaccct" ,
      "[cgt]gggtaaa|tttaccc[acg]",
      "a[act]ggtaaa|tttacc[agt]t",
      "ag[act]gtaaa|tttac[agt]ct",
      "agg[act]taaa|ttta[agt]cct",
      "aggg[acg]aaa|ttt[cgt]ccct",
      "agggt[cgt]aa|tt[acg]accct",
      "agggta[cgt]a|t[acg]taccct",
      "agggtaa[cgt]|[acg]ttaccct")


    // run pattern-counting tasks in parallel
    val countResults = Array.fill(patterns.length)("")
    doWork(
      for ((p,i) <- patterns.zipWithIndex) yield {
        new Work {
          private val m = Pattern.compile(p).matcher(input)
          private var stop = inc
          private var n = 0
          def pri = stop + i
          def run = {
            while (m.find && m.start < stop) n += 1
            stop += inc
            if (m.hitEnd) {
              countResults(i) = p + " " + n
              Seq(Done)
            }
            else {
             n += 1
             Seq(this)
            }
          }
        }
      }
    )
    countResults.foreach(println)

    // replace IUB
    val iub = "Bcgt Dagt Hact Kgt Mac Nacgt Rag Scg Vacg Wat Yct".split(' ').map{ s =>
      Pattern.compile(s.take(1)) -> s.drop(1).mkString("(","|",")").getBytes
    }.toArray

    // run replacement tasks in parallel (in chunks)
    val chunks = Array.tabulate(16){ i =>
      val b = new ByteCS(new Array[Byte]((inc*3)/2))(0)
      b ++= (input.buf, inc*i, math.min(inc*(i+1), input.length))
    }
    val replaced = new MegaCS(chunks)
    val alts = new MegaCS(Array.tabulate(16){ i => new ByteCS(new Array[Byte]((inc*3)/2))(0) })
    class Chunk(var j: Int) extends Work {
      private var i = 0
      def pri = (cores*j+i)*inc
      def run = {
        var ans = replaced.bcs(j)
        var alt = alts.bcs(j)
        alt.length = 0
        val (p,b) = iub(i)
        val m = p.matcher(ans)
        var k = 0
        while (m.find) {
          alt = (alt ++= (ans.buf, k, m.start))
          alt = (alt ++= (b, 0, b.length))
          k = m.end
        }
        alt = (alt ++= (ans.buf, k, ans.length))
        replaced.bcs(j) = alt
        alts.bcs(j) = ans
        if (i+1 >= iub.length) List(Done)
        else if (i != 0 || j+1 >= chunks.length) { i += 1; List(this) }
        else { i +=1; List(this, new Chunk(j+1)) }
      }
    }
    doWork(List(new Chunk(0)))

    // print final results
    printf( "\n%d\n%d\n%d\n", init.length, input.length, replaced.length )
  }
}
