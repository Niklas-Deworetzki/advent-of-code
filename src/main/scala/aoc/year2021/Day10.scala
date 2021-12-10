package aoc.year2021

import aoc.Day
import aoc.strategy.Strategy

import scala.collection.mutable

object Day10 extends Day with Strategy.Shared {
  override type Preprocessed = List[String]
  override type Parsed = List[Either[Char, List[Char]]]

  override def preprocess(input: String): Preprocessed = input.linesIterator.toList

  val pairs = Map[Char, Char](
    '(' -> ')',
    '{' -> '}',
    '<' -> '>',
    '[' -> ']',
  )

  private def parseLine(line: String): Either[Char, List[Char]] = {
    val expectedClosing: mutable.Stack[Char] = new mutable.Stack()

    line.foreach { char =>
      pairs.get(char) match {
        case Some(closing) =>
          expectedClosing.push(closing)
        case None =>
          val expected = expectedClosing.pop()
          if (expected != char) return Left(char)
      }
    }
    Right(expectedClosing.toList)
  }

  override def parse(input: Preprocessed): Parsed = input.map(parseLine)

  override type Solution1 = Long
  override type Solution2 = Long

  val points = Map[Char, Long](
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137,
  )

  override def solve1(input: Parsed): Solution1 =
    input.map {
      case Left(errorChar) => points(errorChar)
      case _ => 0
    }.sum

  val scores = Map[Char, Long](
    ')' -> 1,
    ']' -> 2,
    '}' -> 3,
    '>' -> 4,
  )

  def getScore(chars: Iterable[Char]): Long =
    chars.foldLeft(0.toLong) { (score, char) =>
      score * 5 + scores(char)
    }

  override def solve2(input: Parsed): Solution2 = {
    val allScores = input.flatMap {
      case Right(remaining) => Some(getScore(remaining))
      case Left(_) => None
    }.sorted

    allScores(allScores.length / 2)
  }
}
