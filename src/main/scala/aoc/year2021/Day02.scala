package aoc.year2021

import aoc.Day
import aoc.strategy.Strategy

import cats.kernel.Monoid
import aoc.utils.Pair.monoid

object Day02 extends Day with Strategy.Shared {
  override type Preprocessed = List[String]

  override def preprocess(input: String): Preprocessed = input.linesIterator.toList

  private val RegexForward = """forward (\d+)""".r
  private val RegexUp = """up (\d+)""".r
  private val RegexDown = """down (\d+)""".r

  type Movement = (Int, Int)
  override type Parsed = List[Movement]

  override def parse(input: Preprocessed): Parsed = input.map {
    case RegexForward(n) => (0, n.toInt)
    case RegexUp(n) => (-n.toInt, 0) // up X decreases the depth by X units.
    case RegexDown(n) => (n.toInt, 0) // down X increases the depth by X units.
  }.toList


  override type Solution1 = Int
  override type Solution2 = Int

  override def solve1(input: Parsed1): Solution1 = {
    val resultPosition = Monoid.combineAll(input)
    (resultPosition._1 * resultPosition._2)
  }

  override def solve2(input: Parsed2): Solution2 = {
    var depth = 0
    var x = 0

    input.foldLeft(0) {
      case (aim, (change, 0)) => aim + change
      case (aim, (0, units)) =>
        x += units
        depth += aim * units
        aim
    }

    depth * x
  }
}
