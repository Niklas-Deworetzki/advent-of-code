package aoc.year2015

import aoc.Day
import aoc.strategy.Strategy
import aoc.strategy.Strategy.NoPreprocessing


object Day05 extends Day with Strategy.Default with NoPreprocessing {
  override type Parsed = Iterable[String]

  override def parse(input: String): Parsed =
    input.linesIterator.toList

  override type Solution1 = Int
  override type Solution2 = Int

  override def solve1(input: Parsed): Solution1 =
    input.filter(isNice1).size

  override def solve2(input: Parsed): Solution2 =
    input.filter(isNice2).size

  val containsAtLeast3Vowels = """^.*[aeiou].*[aeiou].*[aeiou].*$""".r
  val containsDoubleLetter = """^.*(.)(\1).*$""".r
  val containsIllegalPairs = """^.*(ab|cd|pq|xy).*$""".r

  val containsTwoLetterPairTwice = """^.*(..).*(\1).*$""".r
  val repeatsWithBetween = """^.*(.).(\1).*$""".r


  private def isNice1(string: String): Boolean =
    containsAtLeast3Vowels.matches(string) &&
      containsDoubleLetter.matches(string) &&
      !containsIllegalPairs.matches(string)

  private def isNice2(string: String): Boolean =
    containsTwoLetterPairTwice.matches(string) &&
      repeatsWithBetween.matches(string)

}
