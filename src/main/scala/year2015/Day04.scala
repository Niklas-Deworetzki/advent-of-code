package aoc
package year2015

import strategy.Strategy

import aoc.strategy.Strategy.NoPreprocessing

import java.security.MessageDigest

object Day04 extends Day(4) with Strategy.ParallelShared with NoPreprocessing {
  type Challenge = (Int, Array[Byte])
  override type Parsed = LazyList[Challenge]

  private val MD5 = MessageDigest.getInstance("MD5")

  override def parse(input: String): Parsed = LazyList.from(1)
    .map(number => (number, MD5.digest((input + number).getBytes)))

  override type Solution1 = Int
  override type Solution2 = Int

  override def solve1(input: Parsed): Solution1 =
    input.take(609043 * 2).find(startsWithZeroes(5))
      .get._1

  override def solve2(input: Parsed): Solution2 =
    input.take(609043 * 2).find(startsWithZeroes(6))
      .get._1

  private def startsWithZeroes(n: Int): Challenge => Boolean =
    if (n % 2 == 0) {
      case (_, bytes) => bytes.view.slice(0, n / 2).forall(_ == 0)
    } else {
      case (_, bytes) => (bytes(n / 2) & 0xF0) == 0 && bytes.view.slice(0, n / 2).forall(_ == 0)
    }
}
