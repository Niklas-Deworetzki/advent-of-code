package aoc.year2021

import aoc.Day
import aoc.strategy.Strategy

object Day08 extends Day with Strategy.Shared {
  override type Preprocessed = List[String]
  override type Parsed = List[(Array[String], Array[String])]

  override def preprocess(input: String): Preprocessed = input.linesIterator.toList

  override def parse(input: Preprocessed): Parsed = {
    for {
      line <- input
      List(signals: String, digits: String) = line.split('|').toList
    } yield (signals.trim.split(' '), digits.trim.split(' '))
  }

  override type Solution1 = Long
  override type Solution2 = Long

  private val filteredLengths = Set(2, 3, 4, 7)

  override def solve1(input: Parsed): Solution1 = {
    for ((_, digits) <- input)
      yield digits.map(_.length).count(filteredLengths.contains)
  }.sum

  private def key(string: String): Set[Int] =
    string.map(_ - 'a').toSet

  val digitSignals: Map[Set[Int], Int] = Map(
    key("abcefg") -> 0,
    key("cf") -> 1,
    key("acdeg") -> 2,
    key("acdfg") -> 3,
    key("bcdf") -> 4,
    key("abdfg") -> 5,
    key("abdefg") -> 6,
    key("acf") -> 7,
    key("abcdefg") -> 8,
    key("abcdfg") -> 9,
  )

  private def findSignalConfiguration(signals: Iterable[String]): Map[Char, Int] = {
    for {
      keys <- ("abcdefg".permutations)
      config = (keys.zipWithIndex).toMap
      if signals.forall(signal => digitSignals.contains(signal.map(config).toSet))
    } yield config
  }.next()


  override def solve2(input: Parsed): Solution2 = {
    for {
      (signal, digits) <- input
      config = findSignalConfiguration(signal)
    } yield digits.map(digit => digitSignals(digit.map(config).toSet)).foldLeft(0) { (current, digit) =>
      current * 10 + digit
    }
  }.sum
}