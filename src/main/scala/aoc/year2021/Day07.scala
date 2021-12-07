package aoc.year2021

import aoc.Day
import aoc.strategy.Strategy

object Day07 extends Day with Strategy.Shared {
  override type Preprocessed = List[String]
  override type Parsed = List[Int]

  override def preprocess(input: String): Preprocessed = input.split(',').toList

  override def parse(input: Preprocessed): Parsed = input.map(_.toInt)

  override type Solution1 = Long
  override type Solution2 = Long

  override def solve1(input: Parsed): Solution1 = {
    for (i <- input.min to input.max)
      yield input.map(_ - i).map(Math.abs).sum
  }.min

  override def solve2(input: Parsed): Solution2 = {
    for (i <- input.min to input.max)
      yield input.map(crabDifference(i)).sum
  }.min

  def crabDifference(target: Int)(origin: Int): Int =
    (1 to Math.abs(target - origin)).sum
}
