package aoc.year2021

import aoc.Day
import aoc.strategy.Strategy

import scala.collection.mutable

object Day11 extends Day with Strategy.Shared {
  override type Preprocessed = Array[Array[Int]]
  override type Parsed = LazyList[Long]

  override def preprocess(input: String): Preprocessed = input.linesIterator.map(line => line.map(_ - '0').toArray).toArray

  override def parse(input: Preprocessed): Parsed = LazyList.continually(step(input))

  override type Solution1 = Long
  override type Solution2 = Long

  val sideLength: Int = 10
  def isValid(n: Int): Boolean = n >= 0 && n < 10

  private def getNeighbours: ((Int, Int)) => Iterable[(Int, Int)] = {
    case (x, y) =>
      for (yd <- -1 to 1; xd <- -1 to 1; if yd != 0 || xd != 0) yield (x + xd, y + yd)
  }

  private def getFlashing(grid: Preprocessed): Set[(Int, Int)] = {
    for (y <- grid.indices; x <- grid(y).indices; if (grid(y)(x) > 9)) yield (x, y)
  }.toSet

  def step(grid: Preprocessed): Long = {
    val flashed: mutable.Set[(Int, Int)] = new mutable.HashSet()

    def doStep: ((Int, Int)) => Unit = {
      case pos@(x, y) if isValid(x) && isValid(y) =>
        if (flashed.contains(pos)) ()
        else {
          grid(y)(x) += 1
          if (grid(y)(x) > 9) {
            flashed.add(pos)
            getNeighbours(pos).foreach(doStep)
          }
        }

      case _ =>
        ()
    }

    for (y <- grid.indices; x <- grid(y).indices)
      doStep((x, y))
    for ((x, y) <- flashed)
      grid(y)(x) = 0
    flashed.size
  }

  override def solve1(input: Parsed): Solution1 =
    input.take(100).sum


  override def solve2(input: Parsed): Solution2 =
    input.indexWhere(_ == sideLength * sideLength) + 1

}