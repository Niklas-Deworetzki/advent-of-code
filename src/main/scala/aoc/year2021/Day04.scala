package aoc.year2021

import aoc.Day
import aoc.strategy.Strategy

object Day04 extends Day with Strategy.Default {
  class Bingo(numbers: Iterable[Int]) {
    val selected: Array[Boolean] = Array.fill(25)(false)
    val indices: Map[Int, Int] = numbers.zipWithIndex.toMap

    def draw(number: Int): Unit =
      indices.get(number).foreach(selected.update(_, true))

    def hasWon: Boolean =
      Bingo.winningsLines.exists(line => line.forall(selected))

    def unmarked: Iterable[Int] =
      for ((number, isSelected) <- numbers.zip(selected); if !isSelected) yield number
  }

  object Bingo {
    private def column(n: Int): Iterable[Int] = for (i <- 0 until 5) yield i * 5 + n
    private def row(n: Int): Iterable[Int] = (5 * n) until (5 * (n + 1))

    val winningsLines: IndexedSeq[Iterable[Int]] = for {
      i <- 0 until 5
      line <- List(column(i), row(i))
    } yield line
  }

  override type Preprocessed = (String, List[List[String]])
  override type Parsed = (List[Int], List[Iterable[Int]])

  override def preprocess(input: String): Preprocessed = {
    val (numbers :: grids) = input.linesIterator.filterNot(_.isEmpty).toList
    (numbers, grids.grouped(5).toList)
  }


  override def parse(input: Preprocessed): Parsed = input match {
    case (rawNumbers, bingos) =>
      val numbers = rawNumbers.split(',').map(_.toInt).toList
      val bingoNumbers = for (bingoDescription <- bingos) yield {
        val nums = bingoDescription.mkString(" ").trim.split("\\s+")
        nums.map(_.toInt).toIterable
      }
      (numbers, bingoNumbers)
  }

  override type Solution1 = Int
  override type Solution2 = Int

  override def solve1(input: Parsed): Solution1 =
    getScoreOfNthWinningBoard(1)(input)

  override def solve2(input: Parsed): Solution2 =
    getScoreOfNthWinningBoard(input._2.size)(input)

  private def getScoreOfNthWinningBoard(n: Int)(input: Parsed): Int = input match {
    case (numbers: List[Int], bingoNumbers: List[Iterable[Int]]) =>
      val bingos = bingoNumbers.map(new Bingo(_))

      var winners: Int = 0
      for (number <- numbers) {
        for (bingo <- bingos if !bingo.hasWon) {
          bingo.draw(number)
          if (bingo.hasWon) {
            winners += 1
          }
          if (winners == n) {
            return number * bingo.unmarked.sum
          }
        }
      }
      throw new IllegalStateException("No winner!")
  }
}
