package aoc.year2021

import aoc.Day
import aoc.strategy.Strategy

import scala.annotation.tailrec
import scala.collection.{SortedSet, mutable}

object Day20 extends Day with Strategy.Default {
  override type Preprocessed = Iterator[String]
  override type Parsed = (IndexedSeq[Boolean], Image)

  type Point = (Int, Int)

  extension (subImage: IndexedSeq[Boolean]) {
    def toIndex: Int = subImage.foldLeft(0) { (index, isSet) =>
      index * 2 + (if (isSet) 1 else 0)
    }
  }

  class Image(withBorder: Boolean = false) {
    private var minX: Int = Int.MaxValue
    private var maxX: Int = Int.MinValue
    private var minY: Int = Int.MaxValue
    private var maxY: Int = Int.MinValue
    private val data: mutable.Set[Point] = new mutable.HashSet()

    def addPoint: Point => Unit = {
      case point@(x, y) =>
        minX = Math.min(minX, x)
        maxX = Math.max(maxX, x)
        minY = Math.min(minY, y)
        maxY = Math.max(maxY, y)
        data.add(point)
    }

    def nextFrameBorder(enhancementTable: IndexedSeq[Boolean]): Boolean =
      if (withBorder) enhancementTable.last else enhancementTable.head

    def pointsAround: Point => IndexedSeq[Boolean] = {
      case (x, y) =>
        for (dy <- -1 to 1; dx <- -1 to 1) yield
          if ((minX to maxX).contains(dx + x) && (minY to maxY).contains(dy + y)) data.contains((dx + x, dy + y))
          else withBorder
    }

    def bounds: Iterable[(Int, Int)] =
      for (x <- (minX - 3) to (maxX + 3); y <- (minY - 3) to (maxY + 3))
        yield (x, y)

    def countLitPixels: Int = if (!withBorder) data.size else ???
  }

  override def preprocess(input: String): Preprocessed = input.linesIterator

  override def parse(input: Preprocessed): Parsed = {
    val algorithm = input.next()
    input.next() // Skip empty line

    val image = new Image()
    for ((line, y) <- input.zipWithIndex; (char, x) <- line.zipWithIndex; if (char == '#'))
      image.addPoint((x, y))

    (algorithm.map(_ == '#'), image)
  }

  override type Solution1 = Long
  override type Solution2 = Long

  def enhance(enhancementTable: IndexedSeq[Boolean], image: Image): Image = {
    val result = new Image(image.nextFrameBorder(enhancementTable))

    for (point <- image.bounds; if enhancementTable(image.pointsAround(point).toIndex))
      result.addPoint(point)

    result
  }

  @tailrec
  def enhanceN(n: Int, enhancementTable: IndexedSeq[Boolean], image: Image): Image =
    if (n == 0) image
    else enhanceN(n - 1, enhancementTable, enhance(enhancementTable, image))

  override def solve1(input: Parsed): Solution1 =
    enhanceN(2, input._1, input._2).countLitPixels

  override def solve2(input: Parsed): Solution2 =
    enhanceN(50, input._1, input._2).countLitPixels
}