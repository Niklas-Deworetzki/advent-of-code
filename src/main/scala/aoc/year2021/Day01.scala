package aoc.year2021

import aoc.Day
import aoc.strategy.Strategy


object Day01 extends Day(1) with Strategy.Shared {
  override type Preprocessed = Iterator[String]
  override type Parsed = List[Int]

  override def preprocess(input: String): Preprocessed = input.linesIterator
  override def parse(input: Preprocessed): Parsed = input.map(_.toInt).toList

  override type Solution1 = Int
  override type Solution2 = Int

  override def solve1(input: Parsed): Solution1 =
    input.sliding(2).count(isIncreasing)

  override def solve2(input: Parsed): Solution2 =
    input.sliding(3).map(_.sum).toList
      .sliding(2).count(isIncreasing)

  private def isIncreasing: List[Int] => Boolean = {
    case List(previous, current) => current > previous
  }

}
