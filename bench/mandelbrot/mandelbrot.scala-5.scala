/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/
   original contributed by Isaac Gouy
   made to use single array and parallelized by Stephen Marsh
   converted to Scala 2.8 by Rex Kerr
   made to use parallel collections and removed synchronized blocks by Steve Vickers
*/
import java.io.BufferedOutputStream

object mandelbrot {
	var size: Int = 0
	var bytesPerRow: Int = 0
	var nextRow = 0
	val limitSquared = 4.0
	val max = 50

	def main(args: Array[String]) {
		size = args(0).toInt
		bytesPerRow = (size+7)/8 // ceiling of (size / 8)

		println("P4\n" + size + " " + size)
		val w = new BufferedOutputStream(System.out)

		(0 until size).par.map (CalculateRow.apply).zipWithIndex.toVector.sortBy (_._2) foreach {
			case (bitmap, y) =>

			w.write(bitmap)
			}

		w.close
	}

	object CalculateRow {
		def apply (y : Int) : Array[Byte] = {
			val bitmap = new Array[Byte] (bytesPerRow);
			var bits = 0
			var bitnum = 0
			var x = 0
			var aindex = 0

			while (x < size) {

				val cr = 2.0 * x / size - 1.5
				val ci = 2.0 * y / size - 1.0

				var zr, tr, zi, ti = 0.0

				var j = max
				do {
					zi = 2.0 * zr * zi + ci
					zr = tr - ti + cr
					ti = zi*zi
					tr = zr*zr

					j = j - 1
				} while (!(tr + ti > limitSquared) && j > 0)

				bits = bits << 1
				if (!(tr + ti > limitSquared)) bits += 1
				bitnum += 1

				if (x == size - 1) {
					bits = bits << (8 - bitnum)
					bitnum = 8
				}

				if (bitnum == 8) {
					bitmap(aindex) = bits.toByte
					aindex += 1
					bits = 0
					bitnum = 0
				}

				x += 1
			}

			bitmap;
		}
	}
}

// vim: set ts=4 sw=4 noet:
