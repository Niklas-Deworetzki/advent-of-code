package aoc.year2023

import aoc.Day
import aoc.strategy.Strategy

import scala.util.matching.Regex

object Day01 extends Day with Strategy.Default with Strategy.NoPreprocessing {
  override type Parsed = List[String]

  override def parse(input: Preprocessed): Parsed =
    input.linesIterator.toList

  override type Solution1 = Int
  override type Solution2 = Int

  override def solve1(input: Parsed): Solution1 =
    input.map(extractCalibrationNumber("[0-9]".r)).sum

  override def solve2(input: Parsed): Solution2 = {
    val regex = translateDigits.keys.mkString("|").r
    input.map(extractCalibrationNumber(regex)).sum
  }

  private val translateDigits: Map[String, Int] = Map(
    "0" -> 0,
    "1" -> 1,
    "2" -> 2,
    "3" -> 3,
    "4" -> 4,
    "5" -> 5,
    "6" -> 6,
    "7" -> 7,
    "8" -> 8,
    "9" -> 9,
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9,
  )

  private def extractCalibrationNumber(regex: Regex)(line: String): Int = {
    val matches = line.tails.flatMap(regex.findPrefixOf).toList
    val firstDigit = translateDigits(matches.head)
    val lastDigit = translateDigits(matches.last)
    (firstDigit * 10) + lastDigit
  }

}
