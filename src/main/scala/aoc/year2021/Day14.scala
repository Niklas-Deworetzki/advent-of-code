package aoc.year2021

import aoc.Day
import aoc.strategy.Strategy

import scala.collection.mutable

object Day14 extends Day with Strategy.Shared {
  override type Preprocessed = Iterator[String]
  override type Parsed = PolymerMachine

  override def preprocess(input: String): Preprocessed = input.linesIterator

  private val Substitution = "(.)(.) -> (.)".r

  override def parse(input: Preprocessed): Parsed = {
    val primer = input.next()
    input.next() // Skip empty line
    val substitutions = input.map {
      case Substitution(pl, pr, res) => ((pl.head, pr.head), res.head)
    }.toMap
    PolymerMachine(primer, substitutions)
  }

  def add(a: Map[Char, Long], b: Map[Char, Long]): Map[Char, Long] = {
    val result = for (c <- 'A' to 'Z')
      yield (c, a.getOrElse(c, 0L) + b.getOrElse(c, 0L))
    result.toMap
  }

  case class PolymerMachine(state: String, substitutions: Map[(Char, Char), Char]) {
    val helper: mutable.Map[(Int, Char, Char), Map[Char, Long]] = new mutable.HashMap()

    def solveFor(n: Int): Map[Char, Long] =
      (state.indices.init).foldLeft(Map(state.head -> 1L)) { (occurrences, i) =>
        add(occurrences, solve(n, state(i), state(i + 1)))
      }

    private def solve(n: Int, a: Char, b: Char): Map[Char, Long] = {
      def solveImpl(n: Int, a: Char, b: Char): Map[Char, Long] = {
        if (n == 0) Map(b -> 1)
        else {
          val substChar = substitutions((a, b))
          val sa = solve(n - 1, a, substChar)
          val sb = solve(n - 1, substChar, b)
          add(sa, sb)
        }
      }

      if (helper.contains((n, a, b))) {
        helper((n, a, b))
      } else {
        val res = solveImpl(n, a, b)
        helper.put((n, a, b), res)
        res
      }
    }
  }

  override type Solution1 = Long
  override type Solution2 = Long

  extension (occurrences: Map[Char, Long]) {
    def score: Long = {
      var maximum = Long.MinValue
      var minimum = Long.MaxValue

      for (c <- 'A' to 'Z') {
        val occurs = occurrences(c)

        if (occurs > 0) minimum = Math.min(occurs, minimum)
        maximum = Math.max(occurs, maximum)
      }

      maximum - minimum
    }
  }

  override def solve1(input: Parsed): Solution1 =
    input.solveFor(10).score

  override def solve2(input: Parsed): Solution2 =
    input.solveFor(40).score
}
