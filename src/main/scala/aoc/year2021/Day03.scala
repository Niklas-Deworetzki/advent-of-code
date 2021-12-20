package aoc.year2021

import aoc.Day
import aoc.strategy.Strategy

import scala.annotation.tailrec

object Day03 extends Day with Strategy.TwoParsers {
  override type Preprocessed = IndexedSeq[IndexedSeq[Char]]

  override def preprocess(input: String): Preprocessed = input.linesIterator.map(_.toIndexedSeq).toIndexedSeq

  private def binary(string: String): Int =
    Integer.parseInt(string, 2)

  override type Parsed1 = IndexedSeq[IndexedSeq[Char]]
  override type Solution1 = Int
  override def parse1(input: Preprocessed): Parsed1 = input.transpose

  override def solve1(input: Parsed1): Solution1 = {
    val gamma = binary(input.map(oneIfBitsAreOn).mkString)
    val epsilon = ~gamma & binary("1" * input.length)
    gamma * epsilon
  }

  private def oneIfBitsAreOn(seq: IndexedSeq[Char]): Char =
    if (seq.count(_ == '1') > seq.length / 2) '1' else '0'


  override type Parsed2 = IndexedSeq[IndexedSeq[Char]]
  override type Solution2 = Int


  override def parse2(input: Preprocessed): Parsed2 = input
  override def solve2(input: Parsed2): Solution2 = {
    val oxygenRating = binary(filterUntilOneRemaining(input, '1').mkString)
    val co2Rating = binary(filterUntilOneRemaining(input, '0').mkString)
    oxygenRating * co2Rating
  }

  @tailrec
  private def filterUntilOneRemaining(input: Parsed2, bit: Char, position: Int = 0): IndexedSeq[Char] = {
    val filtered = input.filter(seq => seq(position) == selectOutputBit(input, bit, position))
    if (input.length == 1) input(0)
    else filterUntilOneRemaining(filtered, bit, position + 1)
  }

  private def selectOutputBit(input: Parsed2, bit: Char, position: Int): Char = {
    val occurrences = input.count(seq => seq(position) == bit)
    if (occurrences * 2 == input.length) bit else if (occurrences * 2 > input.length) '1' else '0'
  }
}
