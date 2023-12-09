package aoc.year2023

import aoc.Day
import aoc.strategy.Strategy

import scala.util.matching.Regex

object Day02 extends Day with Strategy.Default {
  override type Preprocessed = Iterator[String]
  override type Parsed = List[Game]

  override def preprocess(input: String): Preprocessed = input.linesIterator
  override def parse(input: Preprocessed): Parsed = input.map(parseGame).toList

  override type Solution1 = Int
  override type Solution2 = Int

  override def solve1(input: Parsed): Solution1 =
    input.filter(_.isPossibleWithBag(r = 12, g = 13, b = 14))
      .map(_.id)
      .sum

  override def solve2(input: Parsed): Solution2 =
    input.map(_.power)
      .sum


  private val GAME_ID_REGEX = "Game ([0-9]+)".r
  private val RED_REGEX = "([0-9]+) red".r
  private val GREEN_REGEX = "([0-9]+) green".r
  private val BLUE_REGEX = "([0-9]+) blue".r

  private def parseGame(line: String): Game = {
    val id = GAME_ID_REGEX.firstGroupAsInt(line).get
    val sets = for (set <- line.split(';'))
      yield parseCubeSet(set)

    Game(id, sets)
  }

  private def parseCubeSet(set: String): CubeSet = {
    val r = RED_REGEX.firstGroupAsInt(set) getOrElse 0
    val g = GREEN_REGEX.firstGroupAsInt(set) getOrElse 0
    val b = BLUE_REGEX.firstGroupAsInt(set) getOrElse 0
    CubeSet(r, g, b)
  }

  extension (regex: Regex)
    private def firstGroupAsInt(string: String): Option[Int] = for {
      firstMatch <- regex.findFirstMatchIn(string)
      group = firstMatch.group(1)
      value <- group.toIntOption
    } yield value

  case class Game(id: Int, sets: Iterable[CubeSet]) {
    def isPossibleWithBag(r: Int, g: Int, b: Int): Boolean =
      sets.forall(_.isPossibleWithBag(r, g, b))

    def power: Int = {
      val r = sets.map(_.r).max
      val g = sets.map(_.g).max
      val b = sets.map(_.b).max
      r * g * b
    }
  }

  case class CubeSet(r: Int, g: Int, b: Int) {
    def isPossibleWithBag(r: Int, g: Int, b: Int): Boolean =
      this.r <= r && this.g <= g && this.b <= b
  }
}
