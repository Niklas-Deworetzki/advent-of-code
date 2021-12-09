package aoc.year2021

import aoc.Day
import aoc.strategy.Strategy

object Day09 extends Day with Strategy.Shared {
  override type Preprocessed = List[String]
  override type Parsed = Array[Array[Int]]

  override def preprocess(input: String): Preprocessed = input.linesIterator.toList

  override def parse(input: Preprocessed): Parsed =
    input.map(line => line.map(_ - '0').toArray).toArray

  override type Solution1 = Long
  override type Solution2 = Long

  private def isLowestPoint(input: Parsed, y: Int, x: Int): Boolean = {
    val current = input(y)(x)

    val neighbours = for (xd <- List(-1, 0, 1); yd <- List(-1, 0, 1); if xd != 0 || yd != 0)
      yield input.lift(y + yd).flatMap(row => row.lift(x + xd)).getOrElse(9)

    neighbours.forall(n => n > current)
  }

  override def solve1(input: Parsed): Solution1 = {
    for (y <- input.indices; x <- input(y).indices; if isLowestPoint(input, y, x))
      yield 1 + input(y)(x)
  }.sum

  private def replace(from: Int, to: Int)(x: Int): Int =
    if (x == from) to else x

  override def solve2(input: Parsed): Solution2 = {
    var basins: Parsed = Array.ofDim(input.length, input(0).length)
    for (y <- basins.indices; x <- basins(y).indices)
      basins(y)(x) = y * basins(y).length + x

    for (y <- input.indices; x <- input(y).indices) {
      markPath(y, x, input, basins)
    }

    val allBasins = basins.flatMap(identity)

    for (y <- input.indices) {
      for (x <- input(y).indices) {
        printf("%2d ", allBasins(y * input(0).length + x))
      }
      println()
    }

    {
      for (y <- basins.indices; x <- basins(y).indices)
        yield allBasins.count(_ == y * basins(y).length + x)
    }.sorted.reverse.take(3).product
  }

  private def markPath(y: Int, x: Int, input: Parsed, basins: Parsed): Int = {
    if (isLowestPoint(input, y, x)) basins(y)(x)
    else if (input(y)(x) == 9) basins(y)(x)
    else {
      val lowerNeighbours: List[(Int, Int)] = for (
        xd <- List(-1, 0, 1);
        yd <- List(-1, 0, 1);
        if xd != 0 || yd != 0;
        if xd == 0 || yd == 0;
        if input.lift(y + yd).flatMap(row => row.lift(x + xd)).getOrElse(9) < input(y)(x))
      yield (y + yd, x + xd)

      val neighbourValues = for ((ny, nx) <- lowerNeighbours)
        yield markPath(ny, nx, input, basins)

      val ns = lowerNeighbours.mkString(", ")
      val nv = neighbourValues.toSet.mkString(", ")
      if (neighbourValues.toSet.size == 1) {
        basins(y)(x) = neighbourValues.head
      }
      basins(y)(x)
    }
  }
}