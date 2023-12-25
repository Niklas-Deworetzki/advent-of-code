package aoc.year2023

import aoc.Day
import aoc.strategy.Strategy

object Day03 extends Day with Strategy.Default {
  override type Preprocessed = Iterator[String]
  override type Parsed = EngineSchematic

  private val PART_REGEX = "[^.0-9]".r
  private val PART_NUMBER_REGEX = "[0-9]+".r

  override def preprocess(input: String): Preprocessed = input.linesIterator
  override def parse(input: Preprocessed): Parsed = {
    val allParts = Set.newBuilder[(Int, Int)]
    val allNumbers = List.newBuilder[PartNumber]

    for ((line, y) <- input.zipWithIndex) {
      val parts = PART_REGEX.findAllMatchIn(line)
        .map(part => (part.start, y))
      allParts.addAll(parts)

      val partNumbers = PART_NUMBER_REGEX.findAllMatchIn(line)
        .map(part => PartNumber(line.substring(part.start, part.end), part.start, y))
      allNumbers.addAll(partNumbers)
    }
    EngineSchematic(allParts.result(), allNumbers.result())
  }

  override type Solution1 = Int
  override type Solution2 = Int

  override def solve1(input: Parsed): Solution1 =
    input.partNumbers.filter(_.isAdjacentToPart(input.parts))
      .map(_.toInt).sum

  override def solve2(input: Parsed): Solution2 = {
    val associatedParts = for {
      partNumber <- input.partNumbers
      adjacentParts <- partNumber.getAdjacentParts(input.parts)
    } yield (partNumber, adjacentParts)
    val gears = associatedParts.groupMap(_._2)(_._1)
      .view
      .filter(_._2.size == 2)
      .mapValues(_.map(_.toInt))
    gears.values.map(_.product).sum
  }

  class EngineSchematic(val parts: Set[(Int, Int)], val partNumbers: Iterable[PartNumber])

  class PartNumber(text: String, x: Int, y: Int) {
    private lazy val adjacentPositions: Set[(Int, Int)] = {
      val xMin = x - 1
      val xMax = x + text.length

      val above = for (x <- xMin to xMax) yield (x, y - 1)
      val below = for (x <- xMin to xMax) yield (x, y + 1)
      val lSide = (xMin, y)
      val rSide = (xMax, y)
      Set.newBuilder[(Int, Int)]
        .addOne(lSide)
        .addOne(rSide)
        .addAll(above)
        .addAll(below)
        .result()
    }

    def isAdjacentToPart(parts: Set[(Int, Int)]): Boolean =
      adjacentPositions.exists(parts.contains)

    def getAdjacentParts(parts: Set[(Int, Int)]): Set[(Int, Int)] =
      adjacentPositions intersect parts

    def toInt: Int =
      text.toInt
  }

}
