package aoc.year2023

import aoc.Day
import aoc.strategy.Strategy
import aoc.utils.CollectionExtensions.countPairsMatching

object Day24 extends Day with Strategy.Default {
  override type Preprocessed = Iterator[String]
  override type Parsed = IndexedSeq[Hail]

  private val DIGIT = "(-?[0-9]+)".r

  override def preprocess(input: String): Preprocessed = input.linesIterator
  override def parse(input: Preprocessed): Parsed = input.map { line =>
    val List(x, y, z, dx, dy, dz) = DIGIT.findAllIn(line).map(_.toLong).toList
    Hail(
      Point(x, y, z),
      Point(dx, dy, dz)
    )
  }.toIndexedSeq

  override type Solution1 = Int
  override type Solution2 = Int

  override def solve1(input: Parsed): Solution1 =
    input.countPairsMatching { (h1, h2) =>
      willIntersectInXYPlane(h1, h2, 200000000000000L, 400000000000000L)
    }

  override def solve2(input: Parsed): Solution2 = 0

  private def willIntersectInXYPlane(
    h1: Hail,
    h2: Hail,
    boundMin: Long,
    boundMax: Long
  ): Boolean = {
    val cross = h1.vel.x * h2.vel.y - h1.vel.y * h2.vel.x
    if (cross == 0) // Lines are parallel
      return false

    val t2 = -((h2.pos.y - h1.pos.y) * h1.vel.x - (h2.pos.x - h1.pos.x) * h1.vel.y) / cross.toDouble
    if (t2 < 0) // Intersection is in past for h2
      return false

    val intersectionX = h2.pos.x + h2.vel.x * t2
    if (boundMin > intersectionX || intersectionX > boundMax)
      return false
    val intersectionY = h2.pos.y + h2.vel.y * t2
    if (boundMin > intersectionY || intersectionY > boundMax)
      return false

    val t1 = (intersectionX - h1.pos.x) / h1.vel.x
    if (t1 < 0) // Intersection is in past for h1
      return false
    true
  }

  case class Point(x: Long, y: Long, z: Long)
  case class Hail(pos: Point, vel: Point)

}
