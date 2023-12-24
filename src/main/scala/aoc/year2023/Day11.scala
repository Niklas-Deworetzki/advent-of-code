package aoc.year2023

import aoc.Day
import aoc.strategy.Strategy
import aoc.utils.CollectionExtensions.foldPairs

object Day11 extends Day with Strategy.Default {
  override type Preprocessed = Iterator[String]
  override type Parsed = IndexedSeq[Galaxy]

  override def preprocess(input: String): Preprocessed = input.linesIterator
  override def parse(input: Preprocessed): Parsed = {
    val galaxies = for {
      (line, y) <- input.zipWithIndex.to(Iterable)
      (char, x) <- line.zipWithIndex
      if char == '#'
    } yield Galaxy(x, y)

    computeExpansion(galaxies, _.x, _.expandingX = _)
    computeExpansion(galaxies, _.y, _.expandingY = _)
    galaxies.toIndexedSeq
  }

  override type Solution1 = Long
  override type Solution2 = Long

  override def solve1(input: Parsed): Solution1 =
    computeDistances(input, 2)
  override def solve2(input: Parsed): Solution2 =
    computeDistances(input, 1000000)

  private def computeDistances(galaxies: IndexedSeq[Galaxy], expansion: Int): Long =
    galaxies.foldPairs(0L) { (dist, g1, g2) =>
      dist + distance(g1, g2, expansion)
    }

  private def computeExpansion(
    galaxies: Iterable[Galaxy],
    getPos: Galaxy => Int,
    setExpansion: (Galaxy, Int) => Unit
  ): Unit = {
    val groupedGalaxies = galaxies.groupBy(getPos)

    val sortedPositions = groupedGalaxies.keys.toList.sorted
    var totalExpansion = 0
    for ((prevPos, curPos) <- sortedPositions zip sortedPositions.tail) {
      val empty = curPos - prevPos - 1
      if (empty > 0) {
        totalExpansion += empty
      }
      groupedGalaxies(curPos).foreach {
        setExpansion(_, totalExpansion)
      }
    }
  }

  case class Galaxy(x: Int, y: Int) {
    var expandingX: Int = 0
    var expandingY: Int = 0
  }

  def distance(g1: Galaxy, g2: Galaxy, expansion: Int): Int = {
    val xDist = Math.abs((g1.x - g1.expandingX) - (g2.x - g2.expandingX))
      + Math.abs(g1.expandingX - g2.expandingX) * expansion
    val yDist = Math.abs((g1.y - g1.expandingY) - (g2.y - g2.expandingY))
      + Math.abs(g1.expandingY - g2.expandingY) * expansion
    xDist + yDist
  }
}
