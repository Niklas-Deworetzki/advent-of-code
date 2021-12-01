package aoc.year2015

import aoc.Day
import aoc.strategy.Strategy
import aoc.strategy.Strategy.NoPreprocessing

import scala.reflect.ClassTag

object Day06 extends Day(6) with Strategy.ParallelShared {
  override type Preprocessed = Iterator[String]
  override def preprocess(input: String): Preprocessed = input.linesIterator

  override type Parsed = List[Instruction]
  val OnRegex = """turn on (\d+),(\d+) through (\d+),(\d+)""".r
  val OffRegex = """turn off (\d+),(\d+) through (\d+),(\d+)""".r
  val ToggleRegex = """toggle (\d+),(\d+) through (\d+),(\d+)""".r

  override def parse(input: Preprocessed): Parsed = input.map {
    case OnRegex(fx, fy, tx, ty) => On(fx.toInt, fy.toInt, tx.toInt, ty.toInt)
    case OffRegex(fx, fy, tx, ty) => Off(fx.toInt, fy.toInt, tx.toInt, ty.toInt)
    case ToggleRegex(fx, fy, tx, ty) => Toggle(fx.toInt, fy.toInt, tx.toInt, ty.toInt)
  }.toList

  override type Solution1 = Int
  override type Solution2 = Int

  private def solveGeneric[T: ClassTag](input: Parsed, defaultValue: => T)(actor: Instruction => T => T): Array[T] = {
    val field = Array.fill(1000 * 1000)(defaultValue)
    input.foreach { instruction =>
      for (index <- instruction.enclosedIndices)
        field(index) = actor(instruction)(field(index))
    }
    field
  }

  override def solve1(input: Parsed): Solution1 =
    solveGeneric(input, false) {
      case _: On => _ => true
      case _: Off => _ => false
      case _: Toggle => boolean => !boolean
    }.count(identity)

  override def solve2(input: Parsed): Solution2 =
    solveGeneric(input, 0) {
      case _: On => _ + 1
      case _: Off => n => Math.max(0, n - 1)
      case _: Toggle => _ + 2
    }.sum

  sealed abstract class Instruction(fromX: Int, fromY: Int, toX: Int, toY: Int) {
    final def enclosedIndices: Iterable[Int] = for {
      x <- fromX to toX
      y <- fromY to toY
    } yield x + 1000 * y
  }
  case class On(fromX: Int, fromY: Int, toX: Int, toY: Int) extends Instruction(fromX, fromY, toX, toY)
  case class Off(fromX: Int, fromY: Int, toX: Int, toY: Int) extends Instruction(fromX, fromY, toX, toY)
  case class Toggle(fromX: Int, fromY: Int, toX: Int, toY: Int) extends Instruction(fromX, fromY, toX, toY)
}
