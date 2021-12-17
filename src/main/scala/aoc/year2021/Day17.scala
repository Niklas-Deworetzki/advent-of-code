package aoc.year2021

import aoc.Day
import aoc.strategy.Strategy

object Day17 extends Day with Strategy.Shared with Strategy.NoPreprocessing {
  override type Parsed = (Range, Range)

  private val ExtractInfo = """target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)""".r

  override def parse(input: Preprocessed): Parsed = input match {
    case ExtractInfo(x1, x2, y1, y2) => (x1.toInt to x2.toInt, y1.toInt to y2.toInt)
  }

  override type Solution1 = Long
  override type Solution2 = Long

  def reachesTarget(target: Parsed)(initialDx: Int, initialDy: Int): Boolean = {
    var x = 0
    var y = 0
    var dx = initialDx
    var dy = initialDy

    while (y >= target._2.head) {
      x += dx
      y += dy
      dy -= 1
      dx = dx.sign * (Math.max(0, Math.abs(dx) - 1))

      if (target._1.contains(x) && target._2.contains(y))
        return true
    }
    false
  }

  override def solve1(input: Parsed): Solution1 =
    input._2.head * (input._2.head + 1) / 2


  override def solve2(input: Parsed): Solution2 = {
    for (dx <- 0 to input._1.last; dy <- input._2.head to -input._2.head)
      yield (dx, dy)
  }.count(reachesTarget(input))
}