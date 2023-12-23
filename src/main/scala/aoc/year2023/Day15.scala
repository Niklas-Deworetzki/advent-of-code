package aoc.year2023

import aoc.Day
import aoc.strategy.Strategy

object Day15 extends Day with Strategy.TwoParsers {
  override type Preprocessed = Array[String]
  override type Parsed1 = Iterable[String]
  override type Parsed2 = Iterable[Instruction]
  override type Solution1 = Int
  override type Solution2 = Long

  override def preprocess(input: String): Preprocessed =
    input.split(',')

  override def parse1(input: Preprocessed): Parsed1 =
    input
  override def solve1(input: Parsed1): Solution1 =
    input.map(computeHash).sum

  override def parse2(input: Preprocessed): Parsed2 =
    input.map { instruction =>
      if (instruction.last == '-') Remove(instruction.init)
      else {
        val indexOfEq = instruction.indexOf('=')
        val label = instruction.substring(0, indexOfEq)
        val focalLength = instruction.substring(indexOfEq + 1).toInt
        Put(label, focalLength)
      }
    }
  override def solve2(input: Parsed2): Solution2 = {
    val boxes = Array.fill(256)(Box())
    input.foreach {
      case Remove(label) =>
        boxes(computeHash(label)).remove(label)

      case Put(label, focalLength) =>
        boxes(computeHash(label)).put(label, focalLength)
    }
    computeFocusPower(boxes)
  }

  private def computeHash(chars: String): Int =
    chars.foldLeft(0) { (value, char) =>
      ((value + char) * 17) % 256
    }

  private def computeFocusPower(boxes: Array[Box]): Long = {
    var sum: Long = 0
    for {
      (box, boxIndex) <- boxes.zipWithIndex
      (lens, lensIndex) <- box.lenses.zipWithIndex
    } sum += (boxIndex + 1) * (lensIndex + 1) * lens.focalLength
    sum
  }

  private case class Lens(label: String, var focalLength: Int)
  private case class Box(var lenses: List[Lens] = Nil) {
    def remove(label: String): Unit = {
      lenses = lenses.filterNot(_.label == label)
      this.lenses = lenses
    }

    def put(label: String, focalLength: Int): Unit = lenses.find(_.label == label) match {
      case Some(lens) =>
        lens.focalLength = focalLength

      case None =>
        this.lenses = this.lenses appended Lens(label, focalLength)
    }
  }

  sealed trait Instruction {
    val label: String
  }
  private case class Remove(override val label: String) extends Instruction
  private case class Put(override val label: String, focalLength: Int) extends Instruction
}
