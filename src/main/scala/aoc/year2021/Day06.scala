package aoc.year2021

import aoc.Day
import aoc.strategy.Strategy

object Day06 extends Day with Strategy.Shared {
  override type Preprocessed = Array[Long]
  override type Parsed = LazyList[Long]

  override def preprocess(input: String): Preprocessed = {
    val numbers: Preprocessed = Array.ofDim(8 + 1)
    for (n <- input.split(',').map(_.toInt))
      numbers(n) += 1
    numbers
  }

  override def parse(input: Preprocessed): Parsed =
    LazyList.iterate(input)(iterate).map(_.sum)

  override type Solution1 = Long
  override type Solution2 = Long

  private def iterate(numbers: Preprocessed): Preprocessed = {
    val reset = numbers(0)
    for (index <- 0 until 8) {
      numbers(index) = numbers(index + 1)
    }

    numbers(8) = reset
    numbers(6) += reset

    numbers
  }

  override def solve1(input: Parsed): Solution1 =
    input(80)

  override def solve2(input: Parsed): Solution2 =
    input(256)
}
