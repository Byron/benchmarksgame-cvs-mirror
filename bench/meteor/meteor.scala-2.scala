/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by Olof Kraigher
   modified by Sam Thomson
 */

package meteor

import java.lang.Long._


object Meteor extends App {
    val oNum = args.headOption.map(_.toInt)
    val allSolutions = Solver.solve
    val truncated = oNum.fold(allSolutions)(num => allSolutions.take(num))
    val (len, min, max) = Solver.calculateLenMinMax(truncated)
    println(len + " solutions found\n")
    println(min)
    println(max)
}

case class Pos(x: Int, y: Int) {
    import meteor.MaskTools.{height, width}
    import meteor.HexDirection._

    /** Move in the given direction, if possible */
    def move(dir: HexDirection): Option[Pos] = {
        val yMod2 = y % 2
        val (nx, ny) = dir match {
            case E  => (x + 1, y)
            case W  => (x - 1, y)
            case NE => (x + yMod2,     y - 1)
            case NW => (x + yMod2 - 1, y - 1)
            case SE => (x + yMod2,     y + 1)
            case SW => (x + yMod2 - 1, y + 1)
        }
        Pos(nx, ny).checkBounds
    }

    @inline
    private def checkBounds: Option[Pos] = {
        if (0 <= x && x < width &&
            0 <= y && y < height) {
            Some(this)
        } else {
            None
        }
    }

    @inline
    def bit: Long = 1L << (width * y + x)
}

case class HexDirection(private val id: Int) {
    import meteor.HexDirection.numDirections

    /** Rotate a specific amount clockwise */
    def rotate(amount : Int): HexDirection = {
        HexDirection((id + amount) % numDirections)
    }

    /** Flip a direction */
    def flip: HexDirection = HexDirection(numDirections - 1 - id)
}
object HexDirection {
    val numDirections = 6

    val NW = HexDirection(0)
    val W = HexDirection(1)
    val SW = HexDirection(2)
    val SE = HexDirection(3)
    val E = HexDirection(4)
    val NE = HexDirection(5)
}

/**
 * A game piece, given as a sequence of relative HexDirections between its
 * cells
 */
case class Piece(dirs: List[HexDirection], id: Char) {
    private lazy val numRotations: Int = if (id == '0') {
        // we can rotate any solution 180 degrees to get a new solution.
        // so we only calculate the solutions where piece 0 is rotated 1, 2,
        // or 3, and reverse them to generate the solutions where
        // piece 0 is rotated 3, 4, and 5
        HexDirection.numDirections / 2
    } else {
        HexDirection.numDirections
    }

    /** Rotate a specific amount clockwise */
    def rotate(amount: Int): Piece = Piece(dirs.map(_.rotate(amount)), id)

    def flip: Piece = Piece(dirs.map(_.flip), id)

    /* Create all rotated/flipped variations of a piece */
    lazy val allVariations: List[Piece] = {
        for (
            maybeFlipped <- List(this, flip);
            r <- 0 until numRotations
        ) yield {
            maybeFlipped.rotate(r)
        }
    }
    /*
     * Create all rotated/flipped/transposed masks of a piece
     * grouped by the position of the first 1-bit.
     */
    lazy val allMasks: Map[Int, List[Long]] = {
        import meteor.MaskTools.{fromLong, height, width}

        val all: List[Long] =
            for (variation <- allVariations;
                 x <- 0 until width;
                 y <- 0 until height;
                 mask <- variation.toMask(Pos(x,y))
                 if mask.noBadIslands
            ) yield {
                mask
            }
        all.groupBy(mask => (~mask).firstZero())
    }

    /** convert to a bitmask from a starting position */
    def toMask(startPos: Pos): Option[Long] = {
        positions(startPos).map(ps => ps.foldLeft(0L)(_ | _.bit))
    }

    def positions(startPos: Pos): Option[List[Pos]] = {
        dirs.foldLeft(Option(List(startPos))) {
            case (None, dir) => None
            case (Some(ps), dir) => ps.head.move(dir).map(_ :: ps)
        }
    }
}
object Piece {
    import meteor.HexDirection._
    
    val size = 5

    val pieces: List[Piece] = List(
        List(E,  E,  E,  SE),
        List(SE, SW, W,  SW),
        List(W,  W,  SW, SE),
        List(E,  E,  SW, SE),
        List(NW, W,  NW, SE, SW),
        List(E,  E,  NE, W),
        List(NW, NE, NE, W),
        List(NE, SE, E,  NE),
        List(SE, SE, E,  SE),
        List(E,  NW, NW, NW)
    ).zipWithIndex.map({
        case (d, i) => Piece(d, (i + '0').toChar)
    })
}

/** Methods to manipulate Longs as bit masks */
case class MaskTools(mask: Long) {
    import meteor.MaskTools.{fromLong, height, size, width}

    /** Find the position of the first zero bit in mask */
    @inline
    def firstZero(lowerBound: Int = 0): Int = {
        numberOfTrailingZeros(~mask >> lowerBound) + lowerBound
    }

    /**
     * Flood fill starting at the seed, this mask is
     * used as the boundary
     */
    @inline def floodFill(seed: Long): Long = {
        import meteor.MaskTools.fromLong
        var region = 0L
        var growth = seed
        do {
            region = growth
            growth = region.expand & ~mask
        } while (growth != region)
        growth
    }

    /** Check that the mask contains no islands of size not divisible by 5 */
    @inline def noBadIslands: Boolean = {
        var m = mask
        var lastZero = -1
        while (m != MaskTools.full) {
            lastZero = m.firstZero(lastZero + 1)
            val growth = m.floodFill(1L << lastZero)
            if (bitCount(growth) % Piece.size != 0) {
                return false
            }
            m = m | growth
        }
        true
    }

    /** Expand mask by growing it in every direction */
    def expand: Long = {
        import meteor.MaskTools._
        val evens = mask & evenRows
        val odds = mask & oddRows
        val toE = (mask & ~eastBorder) << 1
        val toW = (mask & ~westBorder) >> 1
        val toNW = (odds >> width)  | ((evens & ~westBorder) >> (width + 1))
        val toNE = (evens >> width ) | ((odds & ~eastBorder) >> (width - 1))
        val toSW = (odds << width)  | ((evens & ~westBorder) << (width - 1))
        val toSE = (evens << width) | ((odds & ~eastBorder) << (width + 1))
        (mask | toE | toW | toNW | toNE | toSW | toSE) & full
    }

    def rotate180: Long = {
        // just reverse the order of the bits
        val lengthLess1: Int = size - 1
        val reversed: Iterator[Long] =
            for (idx <- (0 until size).iterator) yield {
                ((mask >> idx) & 1) << (lengthLess1 - idx)
            }
        reversed.foldLeft(0L)(_ | _)
    }

    def nonZeroPositions: Iterator[Pos] = {
        for (x <- (0 until width).iterator;
             y <- (0 until height).iterator
             if (mask & Pos(x, y).bit) != 0) yield {
            Pos(x, y)
        }
    }

    override def toString: String = {
        Solution(Map('1' -> mask)).toString
    }

    implicit def toLong: Long = mask
}

object MaskTools {
    val width = 5
    val height = 10
    val size = width * height

    val empty = 0L
    val full = (1L << size) - 1
    val firstRow: Long = (1 << width) - 1
    val evenRows = {
        val evens =
            for (rowIdx <- 0 until height by 2) yield {
                firstRow << (rowIdx * width)
            }
        evens.foldLeft(0L)(_ | _)
    }
    val oddRows = evenRows << width
//    val westBorder = {
//        val westBorderBits =
//            for (rowIdx <- 0 until height) yield {
//                1L << (rowIdx * width)
//            }
//        westBorderBits.foldLeft(0L)(_ | _)
//    }
    val westBorder = 0x210842108421L
    val eastBorder = westBorder << (width - 1)

    implicit def fromLong(mask: Long): MaskTools = MaskTools(mask)
}

case class Solution(pieces: Map[Char, Long]) {
    import meteor.MaskTools.{fromLong, height, width}

    def rotate180: Solution = Solution(pieces.mapValues(_.rotate180))

    override def toString: String = {
        val board : Array[Array[Char]] =
            Array.fill(height, width)('-')
        for ((piece, mask) <- pieces;
             Pos(x, y) <- mask.nonZeroPositions) {
            board(y)(x) = piece
        }
        board.zipWithIndex.map({
            case (row, i) => Solution.indent(i) + row.mkString(" ")
        }).mkString("\n") + "\n"
    }
}
object Solution {
    /** Indent odd rows */
    @inline private def indent(y : Int) = if (y % 2 == 1) " " else ""
}

/** Solves the meteor puzzle */
object Solver {
    import meteor.MaskTools.fromLong

    /** Solve the meteor puzzle */
    @inline def solve: Iterator[Solution] = {
        solveAux(MaskTools.empty, 0, Solution(Map()), Piece.pieces)
    }

    /** Recursive search */
    private def solveAux(boardState: Long,
                         firstZeroLowerBound: Int,
                         partialSoln: Solution,
                         piecesRemaining: List[Piece]): Iterator[Solution] = {
        // try to fill empty spots in order
        val firstZeroIdx = boardState.firstZero(firstZeroLowerBound)
        if (piecesRemaining.isEmpty) {
            // done
            Iterator(partialSoln, partialSoln.rotate180)
        } else {
            piecesRemaining.zipWithIndex.iterator flatMap {
                case (piece, pIdx) =>
                    val remaining = piecesRemaining.slice(0, pIdx) ++
                        piecesRemaining.slice(pIdx + 1, piecesRemaining.size)
                    for (mask <- piece.allMasks.getOrElse(firstZeroIdx, Nil)
                         if (boardState & mask) == 0L;
                         newBoardState = boardState | mask;
//                         if newBoardState.noIslands;
                         solution <- solveAux(
                                newBoardState,
                                firstZeroIdx + 1,
                                Solution(partialSoln.pieces + (piece.id -> mask)),
                                remaining)
                    ) yield {
                        solution
                    }
            }
        }
    }

    @inline def calculateLenMinMax(solutions: Iterator[Solution]): (Int, String, String) = {
        solutions.foldLeft (0, "9", "0") { case ((l, mn, mx), solution) =>
            val solutionStr = solution.toString
            (l + 1, Seq(mn, solutionStr).min, Seq(mx, solutionStr).max)
        }
    }
}
