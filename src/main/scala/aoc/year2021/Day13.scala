package aoc.year2021

import aoc.Day
import aoc.strategy.Strategy

object Day13 extends Day with Strategy.Default {
  override type Preprocessed = List[String]
  override type Parsed = FoldingInstructions

  override def preprocess(input: String): Preprocessed = input.linesIterator.toList

  private val Point = "(\\d+),(\\d+)".r
  private val HorizontalCut = "fold along x=(\\d+)".r
  private val VerticalCut = "fold along y=(\\d+)".r

  override def parse(input: Preprocessed): Parsed = {
    val points = input.takeWhile(_.nonEmpty) map {
      case Point(x, y) => (x.toInt, y.toInt)
    }
    val folds = input.drop(points.length + 1) map {
      case HorizontalCut(coord) => FoldingCut(coord.toInt, true)
      case VerticalCut(coord) => FoldingCut(coord.toInt, false)
    }

    FoldingInstructions(points.toSet, folds)
  }

  case class FoldingInstructions(paper: Set[(Int, Int)], folds: List[FoldingCut])
  case class FoldingCut(coordinate: Int, horizontal: Boolean) {
    def asPoint: (Int, Int) =
      if (horizontal) (coordinate, Int.MaxValue)
      else (Int.MaxValue, coordinate)
  }

  override type Solution1 = Int
  override type Solution2 = String

  private def foldCoordinate(coordinate: Int, cut: Int): Int =
    if (coordinate > cut) cut - (coordinate - cut)
    else coordinate


  def foldPoints(points: Set[(Int, Int)], cut: FoldingCut): Set[(Int, Int)] = {
    val (cx, cy) = cut.asPoint
    points.map {
      case (x, y) => (foldCoordinate(x, cx), foldCoordinate(y, cy))
    }
  }


  override def solve1(input: Parsed): Solution1 =
    foldPoints(input.paper, input.folds.head).size

  override def solve2(input: Parsed): Solution2 =
    paper(input.folds.foldLeft(input.paper)(foldPoints))


  private def paper(points: Set[(Int, Int)]): String = {
    val maxX = points.map(_._1).max
    val maxY = points.map(_._2).max

    val lines = for (y <- 0 to maxY) yield {
      val chars = for (x <- 0 to maxX)
        yield if (points.contains((x, y))) '#' else ' '
      chars.mkString
    }
    lines.mkString("\n")
  }
}
