package aoc.year2021

import aoc.Day
import aoc.strategy.Strategy

import scala.annotation.tailrec
import scala.util.parsing.combinator.{ImplicitConversions, Parsers, RegexParsers}

object Day18 extends Day with Strategy.Default with Parsers with RegexParsers with ImplicitConversions {
  override type Preprocessed = Iterator[String]
  override type Parsed = IndexedSeq[Number]

  sealed trait Number
  case class Constant(n: Int) extends Number {
    override def toString: String = n.toString
  }
  case class Pair(lhs: Number, rhs: Number) extends Number {
    override def toString: String = s"[$lhs,$rhs]"
  }

  val constant: Parser[Constant] = """\d""".r ^^ Integer.parseInt ^^ Constant.apply
  val pair: Parser[Pair] = ("[" ~> number) ~ ("," ~> number <~ "]") ^^ Pair.apply

  def number: Parser[Number] = constant | pair

  override def preprocess(input: String): Preprocessed = input.linesIterator

  override def parse(input: Preprocessed): Parsed = {
    for (line <- input)
      yield parse(number, line).get
  }.toIndexedSeq

  sealed trait Explode
  case class FixBoth(lhs: Int, rhs: Int) extends Explode
  case class FixLeft(lhs: Int) extends Explode
  case class FixRight(rhs: Int) extends Explode
  case class Fixed(changed: Boolean) extends Explode

  private def addLeftmost(value: Int): Number => Number = {
    case Constant(n) => Constant(n + value)
    case Pair(lhs, rhs) => Pair(addLeftmost(value)(lhs), rhs)
  }

  private def addRightmost(value: Int): Number => Number = {
    case Constant(n) => Constant(n + value)
    case Pair(lhs, rhs) => Pair(lhs, addRightmost(value)(rhs))
  }

  def explode(number: Number, depthRemaining: Int = 4): (Explode, Number) = number match {
    case Pair(Constant(x), Constant(y)) if depthRemaining == 0 => (FixBoth(x, y), Constant(0))
    case constant: Constant => (Fixed(false), constant)
    case Pair(lhs, rhs) =>
      val (lhsExplode, lhsResult) = explode(lhs, depthRemaining - 1)
      lhsExplode match {
        case FixBoth(lhsconst, rhsconst) =>
          (FixLeft(lhsconst), Pair(lhsResult, addLeftmost(rhsconst)(rhs)))
        case FixRight(rhsconst) =>
          (Fixed(true), Pair(lhsResult, addLeftmost(rhsconst)(rhs)))
        case fixed@(FixLeft(_) | Fixed(true)) =>
          (fixed, Pair(lhsResult, rhs))
        case Fixed(false) =>
          val (rhsExplode, rhsResult) = explode(rhs, depthRemaining - 1)
          rhsExplode match {
            case FixBoth(lhsconst, rhsconst) =>
              (FixRight(rhsconst), Pair(addRightmost(lhsconst)(lhsResult), rhsResult))
            case FixLeft(lhsconst) =>
              (Fixed(true), Pair(addRightmost(lhsconst)(lhsResult), rhsResult))
            case fixed: (Fixed | FixRight) =>
              (fixed, Pair(lhsResult, rhsResult))
          }
      }
  }

  def split: Number => (Boolean, Number) = {
    case Constant(n) if n >= 10 =>
      (true, Pair(Constant(n / 2), Constant((n + 1) / 2)))
    case constant: Constant =>
      (false, constant)
    case Pair(lhs, rhs) =>
      val (lhsChanged, lhsResult) = split(lhs)
      if (!lhsChanged) {
        val (rhsChanged, rhsResult) = split(rhs)
        (rhsChanged, Pair(lhsResult, rhsResult))
      } else {
        (true, Pair(lhsResult, rhs))
      }
  }

  @tailrec
  def reduce(number: Number): Number = {
    val (ex, exploded) = explode(number)
    ex match {
      case Fixed(false) =>
        val (sp, splitted) = split(exploded)
        if (sp) reduce(splitted)
        else splitted
      case _ =>
        reduce(exploded)
    }
  }

  def add(x: Number, y: Number): Number =
    reduce(Pair(x, y))

  def magnitude: Number => Int = {
    case Constant(n) => n
    case Pair(lhs, rhs) => 3 * magnitude(lhs) + 2 * magnitude(rhs)
  }

  override type Solution1 = Int
  override type Solution2 = Int

  override def solve1(input: Parsed): Solution1 =
    magnitude(input.reduce(add))

  override def solve2(input: Parsed): Solution2 = {
    for (x <- input.indices; y <- input.indices; if x != y)
      yield magnitude(add(input(x), input(y)))
  }.max
}