package aoc.year2021

import aoc.Day
import aoc.strategy.Strategy

object Day22 extends Day with Strategy.Default {
  override type Preprocessed = Iterator[String]
  override type Parsed = Seq[Instruction]

  type Instruction = (Boolean, Cube)
  case class Cube(x1: Long, y1: Long, z1: Long, x2: Long, y2: Long, z2: Long) {
    def volume: Long =
      (x2 - x1) * (y2 - y1) * (z2 - z1)

    def subtract(other: Cube): Iterable[Cube] =
      if (other.includes(this)) Nil // Nothing remains
      else if (!this.intersects(other)) Some(this) // Nothing is removed
      else {
        val xSplits = List(other.x1, other.x2).filter(x => x1 < x && x < x2)
        val ySplits = List(other.y1, other.y2).filter(y => y1 < y && y < y2)
        val zSplits = List(other.z1, other.z2).filter(z => z1 < z && z < z2)

        val xRange = x1 +: xSplits :+ x2
        val yRange = y1 +: ySplits :+ y2
        val zRange = z1 +: zSplits :+ z2

        val results = for ((xl, xh) <- xRange zip xRange.tail;
                           (yl, yh) <- yRange zip yRange.tail;
                           (zl, zh) <- zRange zip zRange.tail)
        yield Cube(xl, yl, zl, xh, yh, zh)

        results.filterNot(other.includes)
      }


    def includes(other: Cube): Boolean =
      this.x1 <= other.x1 && this.x2 >= other.x2 &&
        this.y1 <= other.y1 && this.y2 >= other.y2 &&
        this.z1 <= other.z1 && this.z2 >= other.z2

    private def intersectImpl(other: Cube): Boolean =
      this.x1 <= other.x2 && this.x2 >= other.x1 &&
        this.y1 <= other.y2 && this.y2 >= other.y1 &&
        this.z1 <= other.z2 && this.z2 >= other.z1

    def intersects(other: Cube): Boolean =
      this.intersectImpl(other) || other.intersectImpl(this)

    def intersection(other: Cube): Option[Cube] =
      if (intersects(other)) Some(Cube(
        Math.max(this.x1, other.x1), Math.max(this.y1, other.y1), Math.max(this.z1, other.z1),
        Math.min(this.x2, other.x2), Math.min(this.y2, other.y2), Math.min(this.z2, other.z2)
      ))
      else None
  }


  override def preprocess(input: String): Preprocessed = input.linesIterator

  private val SequenceLine = """(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)""".r

  override def parse(input: Preprocessed): Parsed = input.map {
    case SequenceLine("on", x1, x2, y1, y2, z1, z2) => (true, Cube(x1.toLong, y1.toLong, z1.toLong, x2.toLong + 1, y2.toLong + 1, z2.toLong + 1))
    case SequenceLine("off", x1, x2, y1, y2, z1, z2) => (false, Cube(x1.toLong, y1.toLong, z1.toLong, x2.toLong + 1, y2.toLong + 1, z2.toLong + 1))
  }.toSeq


  def countEnabled(parsed: Parsed): Long = {
    var enabled: List[Cube] = Nil

    parsed.foreach { case (toggleOn, cube) =>
      enabled = enabled.flatMap(_.subtract(cube))
      if (toggleOn)
        enabled = cube :: enabled
    }

    enabled.map(_.volume).sum
  }

  override type Solution1 = Long
  override type Solution2 = Long

  override def solve1(input: Parsed): Solution1 = {
    val boundingBox = Cube(-50, -50, -50, 51, 51, 51)
    val boundInput = input.flatMap { (toggle, cube) =>
      boundingBox.intersection(cube).map((toggle, _))
    }
    countEnabled(boundInput)
  }

  override def solve2(input: Parsed): Solution2 =
    countEnabled(input)

}