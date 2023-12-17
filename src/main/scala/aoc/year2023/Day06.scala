package aoc.year2023

import aoc.Day
import aoc.strategy.Strategy

object Day06 extends Day with Strategy.TwoParsers {
  override type Preprocessed = (List[String], List[String])
  override type Parsed1 = List[Race]
  override type Parsed2 = Race
  override type Solution1 = Long
  override type Solution2 = Long

  private val DIGITS = "[0-9]+".r

  private def extractDigits(text: String): List[String] =
    DIGITS.findAllIn(text).toList

  override def preprocess(input: String): (List[String], List[String]) = {
    val lines = input.linesIterator
    val durations = extractDigits(lines.next())
    val distances = extractDigits(lines.next())
    (durations, distances)
  }

  override def parse1(input: Preprocessed): Parsed1 = input match {
    case (distances, durations) =>
      (distances.map(_.toLong) zip durations.map(_.toLong))
        .map(Race.apply)
  }

  override def solve1(inputs: Parsed1): Solution1 =
    inputs.map(_.countWinningConfigurations())
      .product


  override def parse2(input: Preprocessed): Parsed2 = input match {
    case (distanceDigits, durationDigits) =>
      val distance = distanceDigits.mkString.toLong
      val duration = durationDigits.mkString.toLong
      Race(distance, duration)
  }

  override def solve2(input: Parsed2): Solution2 =
    input.countWinningConfigurations()

  case class Race(duration: Long, distance: Long) {
    /**
     * All winning configurations are of form `p * (duration - p) > distance`,
     * where `p` is the duration we press down the accelerator `0 <= p <= duration`.
     *
     * We are interested in the longest and shorted duration to press down, i.e.
     * those values `p` where `p * (duration - p) - distance = 0`.
     * This can be re-ordered to `p² - (duration * p) + distance = 0` and then
     * solved as `duration / 2 ± sqrt ( (duration / 2)² - distance )`
     */
    def countWinningConfigurations(): Long = {
      val epsilon = 0.00001 // Added/Subtracted from calculated number, as we want to beat the record, not match it.
      val min = duration / 2.0 - Math.sqrt((duration * duration) / 4.0 - distance) + epsilon
      val max = duration / 2.0 + Math.sqrt((duration * duration) / 4.0 - distance) - epsilon

      val minNormalized = Math.max(min.ceil, 0).longValue
      val maxNormalized = Math.min(max.floor, duration).longValue

      maxNormalized - minNormalized + 1
    }
  }
}
