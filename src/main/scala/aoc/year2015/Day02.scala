package aoc.year2015

import aoc.Day
import aoc.strategy.Strategy

object Day02 extends Day(2) with Strategy.Shared {
  override type Preprocessed = Iterable[String]
  override type Parsed = Iterable[(Int, Int, Int)]

  override def preprocess(input: String): Iterable[String] = input.linesIterator.toList

  private val Line = """(\d+)x(\d+)x(\d+)""".r
  override def parse(input: Iterable[String]): Iterable[(Int, Int, Int)] = input.map {
    case Line(l, w, h) => (l.toInt, w.toInt, h.toInt)
  }

  override type Solution1 = Int
  override type Solution2 = Int

  private def wrappingPaper(l: Int, w: Int, h: Int): Int = {
    val areas = List(l * w, w * h, h * l)
    areas.sum * 2 + areas.min
  }

  private def ribbon(l: Int, w: Int, h: Int): Int = {
    val sides = List(l, w, h)
    2 * (sides.sum - sides.max) + sides.product
  }

  override def solve1(input: Parsed): Solution1 =
    input.map(Function.tupled(wrappingPaper)).sum

  override def solve2(input: Parsed): Solution2 =
    input.map(Function.tupled(ribbon)).sum
}
