package aoc.year2015

import aoc.Day
import aoc.strategy.Strategy
import aoc.strategy.Strategy.NoPreprocessing

object Day01 extends Day(1) with Strategy.Shared with NoPreprocessing {
  override type Parsed = Iterable[Int]
  override type Solution1 = Int
  override type Solution2 = Int

  def parse(input: Preprocessed): Parsed = input.map {
    case '(' => +1
    case ')' => -1
  }

  def solve1(input: Parsed): Solution1 = input.sum
  def solve2(input: Parsed): Solution2 = input.scanLeft(0)(_ + _).takeWhile(_ >= 0).size
}
