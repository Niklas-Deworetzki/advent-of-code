package aoc.year2015

import aoc.Day
import aoc.strategy.Strategy
import aoc.strategy.Strategy.NoPreprocessing
import aoc.utils.Crypto

import java.security.MessageDigest

object Day04 extends Day(4) with Strategy.ParallelShared with NoPreprocessing {
  type Challenge = (Int, String)
  override type Parsed = LazyList[Challenge]


  override def parse(input: String): Parsed = Crypto.genMD5(number => input + number, start = 1)

  override type Solution1 = Int
  override type Solution2 = Int

  override def solve1(input: Parsed): Solution1 =
    input.find(startsWithZeroes(5))
      .get._1

  override def solve2(input: Parsed): Solution2 =
    input.find(startsWithZeroes(6))
      .get._1

  private def startsWithZeroes(n: Int)(ignored: Int, string: String): Boolean =
    string.view.slice(0, n).forall(_ == '0')

}
