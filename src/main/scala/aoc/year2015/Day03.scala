package aoc.year2015

import aoc.Day
import aoc.strategy.Strategy
import aoc.strategy.Strategy.NoPreprocessing

import cats.kernel.Monoid

object Day03 extends Day(3) with Strategy.Shared with NoPreprocessing {
  override type Parsed = Seq[(Int, Int)]

  given monoid: Monoid[(Int, Int)] = new Monoid[(Int, Int)] {
    override def empty: (Int, Int) = (0, 0)
    def combine(l: (Int, Int), r: (Int, Int)): (Int, Int) =
      (l._1 + r._1, l._2 + r._2)
  }

  private val Directions: Map[Char, (Int, Int)] = Map(
    'v' -> (-1, 0),
    '>' -> (0, 1),
    '^' -> (1, 0),
    '<' -> (0, -1)
  )

  override def parse(input: String): Parsed = input.map(Directions)

  override type Solution1 = Int
  override type Solution2 = Int

  override def solve1(input: Parsed): Solution1 =
    input.scanLeft(monoid.empty)(monoid.combine).toSet.size


  override def solve2(input: Parsed): Solution2 = {
    val (santa, robot) = every2(input)
    val santaPos = santa.scanLeft(monoid.empty)(monoid.combine)
    val robotPos = robot.scanLeft(monoid.empty)(monoid.combine)
    ((santaPos.toSet) union (robotPos.toSet)).size
  }

  private def every2[A](seq: Seq[A]): (Seq[A], Seq[A]) =
    seq.zipWithIndex.partitionMap { case (element, i) =>
      if (i % 2 == 0) Left(element) else Right(element)
    }
}
