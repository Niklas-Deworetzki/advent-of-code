package aoc.year2021

import aoc.Day
import aoc.strategy.Strategy

object Day05 extends Day with Strategy.Default {
  override type Preprocessed = List[String]
  override type Parsed = List[Line]

  case class Line(x1: Int, y1: Int, x2: Int, y2: Int) {
    def isVertical: Boolean = y1 == y2
    def isHorizontal: Boolean = x1 == x2
    def isDiagonal: Boolean = Math.abs(x1 - x2) == Math.abs(y1 - y2)

    def iteratePoints: Iterable[(Int, Int)] =
      if (isHorizontal) for (y <- Math.min(y1, y2) to Math.max(y1, y2)) yield (x1, y)
      else if (isVertical) for (x <- Math.min(x1, x2) to Math.max(x1, x2)) yield (x, y1)
      else if (isDiagonal) for (d <- 0 to Math.abs(x1 - x2)) yield (x1 + d * Math.signum(x2 - x1).toInt, y1 + d * Math.signum(y2 - y1).toInt)
      else ???
  }

  private val LineRegex = """(\d+),(\d+) -> (\d+),(\d+)""".r

  override def preprocess(input: String): Preprocessed = input.linesIterator.toList
  override def parse(input: Preprocessed): Parsed = input.map {
    case LineRegex(x1, y1, x2, y2) => Line(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
  }


  override type Solution1 = Int
  override type Solution2 = Int

  override def solve1(input: Parsed): Solution1 = {
    val maxX = input.flatMap(x => List(x.x1, x.x2)).max
    val maxY = input.flatMap(x => List(x.y1, x.y2)).max

    val field: Array[Array[Int]] = Array.ofDim(maxY + 1, maxX + 1)

    for (line <- input if line.isVertical || line.isHorizontal) {
      for ((x, y) <- line.iteratePoints)
        field(y)(x) += 1
    }

    field.flatMap(identity).count(_ >= 2)
  }

  override def solve2(input: Parsed): Solution2 = {
    val maxX = input.flatMap(x => List(x.x1, x.x2)).max
    val maxY = input.flatMap(x => List(x.y1, x.y2)).max

    val field: Array[Array[Int]] = Array.ofDim(maxY + 1, maxX + 1)

    for (line <- input) {
      for ((x, y) <- line.iteratePoints)
        field(y)(x) += 1
    }

    field.flatMap(identity).count(_ >= 2)
  }
}
