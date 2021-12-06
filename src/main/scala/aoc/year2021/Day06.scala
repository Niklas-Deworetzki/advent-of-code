package aoc.year2021

import aoc.Day
import aoc.strategy.Strategy

object Day06 extends Day with Strategy.Shared {
  override type Preprocessed = List[String]
  override type Parsed = Array[Long]

  override def preprocess(input: String): Preprocessed = input.split(',').toList
  override def parse(input: Preprocessed): Parsed = {
    val numbers: Parsed = Array.ofDim(10)
    for (n <- input.map(_.toInt)) numbers(n) += 1
    numbers
  }

  override type Solution1 = Long
  override type Solution2 = Long

  private def iterate(numbers: Parsed): Parsed = {
    val reset = numbers(0)
    for (index <- 0 until 9) {
      numbers(index) = numbers(index + 1)
    }

    numbers(8) = reset
    numbers(6) += reset

    numbers
  }

  private def unfold(numbers: List[Int]): List[Int] = {
    val m = numbers.map(_ - 1)
    List(m.map(x => if (x == -1) 6 else x), List.fill(m.count(_ == -1))(8)).flatten
  }

  override def solve1(input: Parsed): Solution1 =
    LazyList.iterate(input.clone())(iterate)(80).sum

  override def solve2(input: Parsed): Solution2 =
    LazyList.iterate(input.clone())(iterate)(256).sum
}
