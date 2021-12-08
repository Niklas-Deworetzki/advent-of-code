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
      List(segments: String, switches: String) = line.split('|').toList
    } yield (segments.trim.split(' '), switches.trim.split(' '))
  }

  override type Solution1 = Long
  override type Solution2 = Long

  override def solve1(input: Parsed): Solution1 = {
    for ((_, switches) <- input)
      yield switches.count { switch =>
        switch.length == 2 || switch.length == 3 || switch.length == 4 || switch.length == 7
      }
  }.sum

  private def key(string: String): Set[Int] =
    string.map(_ - 'a').toSet

  val digits: Map[Set[Int], Int] = Map(
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

  private def findSignalConfiguration(signals: Array[String]): Map[Char, Int] = {
    for {
      keys <- ("abcdefg".permutations)
      config = (keys.zipWithIndex).toMap
      if signals.forall(signal => digits.contains(signal.map(config).toSet))
    } yield config
  }.next()


  override def solve2(input: Parsed): Solution2 = {
    for {
      (signal, switches) <- input
      config = findSignalConfiguration(signal)
    } yield
      switches.map(digit => digits(digit.map(config).toSet)).mkString("").toInt

  }.sum
}