package aoc.year2021

import aoc.Day
import aoc.strategy.Strategy

import scala.collection.mutable

object Day24 extends Day with Strategy.Parallel {
  override type Preprocessed = Iterator[String]

  override def preprocess(input: String): Iterator[String] = input.linesIterator

  private val regex =
    """
      |inp w
      |mul x 0
      |add x z
      |mod x 26
      |div z (-?\d+)
      |add x (-?\d+)
      |eql x w
      |eql x 0
      |mul y 0
      |add y 25
      |mul y x
      |add y 1
      |mul z y
      |mul y 0
      |add y w
      |add y (-?\d+)
      |mul y x
      |add z y
""".stripMargin.linesIterator.filterNot(_.isBlank).mkString(" ")

  private val Segments = regex.r

  def mkFunction(k1: Int, k2: Int, k3: Int)(i: Int, z: Int): Int = {
    val w = i
    val x = z % 26 + k2
    if (w == x) z / k1
    else ((z / k1) * 26) + w + k3
  }

  override type Parsed = List[(Int, Int, Int)]
  override def parse(input: Iterator[String]): Parsed = input.grouped(18).map(_.mkString(" ")).map {
    case Segments(a, b, c) => (a.toInt, b.toInt, c.toInt)
  }.toList

  override type Solution1 = String
  override type Solution2 = String

  def tryAll(z: Int, parameters: List[(Int, Int, Int)], digits: Iterable[Int],
             acc: List[Int] = Nil, memory: mutable.Set[(Int, Int)] = new mutable.HashSet()): Option[List[Int]] = {
    if (parameters.isEmpty) if (z == 0) Some(acc.reverse) else None
    else if (!memory.add((z, parameters.size))) None
    else {
      val step = Function.tupled(mkFunction)(parameters.head)

      for (i <- digits.iterator) {
        tryAll(step(i, z), parameters.tail, digits, i :: acc, memory) match {
          case Some(result) => return Some(result)
          case None =>
        }
      }
      None
    }
  }

  override def solve1(input: Parsed): Solution1 =
    tryAll(0, input, (1 to 9).reverse).get.mkString

  override def solve2(input: Parsed): Solution2 =
    tryAll(0, input, 1 to 9).get.mkString

}
