package aoc.year2021

import aoc.Day
import aoc.strategy.Strategy

object Day25 extends Day with Strategy.Default {
  override type Preprocessed = Iterator[String]
  override type Parsed = Array[Array[Seafloor]]

  sealed trait Seafloor {
    override def toString: String = this match {
      case Empty => "."
      case CucumberEast => ">"
      case CucumberSouth => "v"
    }
  }
  case object Empty extends Seafloor
  case object CucumberEast extends Seafloor
  case object CucumberSouth extends Seafloor

  override def preprocess(input: String): Preprocessed = input.linesIterator
  override def parse(input: Preprocessed): Parsed = {
    for (line <- input)
      yield line.map {
        case '.' => Empty
        case '>' => CucumberEast
        case 'v' => CucumberSouth
      }.toArray
  }.toArray

  def stepEast(from: Parsed, to: Parsed): Boolean = {
    var changed = false

    for (y <- from.indices) {
      var x = 0
      while (x < from(y).length) {
        val target = (x + 1) % from(y).length
        if (from(y)(x) == CucumberEast && from(y)(target) == Empty) {
          to(y)(x) = Empty
          to(y)(target) = CucumberEast

          changed = true
          x += 2
        } else {
          to(y)(x) = from(y)(x)

          x += 1
        }
      }
    }

    changed
  }

  def stepSouth(from: Parsed, to: Parsed): Boolean = {
    var changed = false

    for (x <- from.head.indices) {
      var y = 0
      while (y < from.length) {
        val target = (y + 1) % from.length
        if (from(y)(x) == CucumberSouth && from(target)(x) == Empty) {
          to(y)(x) = Empty
          to(target)(x) = CucumberSouth

          changed = true
          y += 2
        } else {
          to(y)(x) = from(y)(x)

          y += 1
        }
      }
    }

    changed
  }

  def step(seafloor: Parsed, auxiliary: Parsed): Boolean =
    stepEast(seafloor, auxiliary) | stepSouth(auxiliary, seafloor)

  override type Solution1 = Int
  override type Solution2 = Int

  override def solve1(input: Parsed): Solution1 = {
    val aux: Parsed = Array.ofDim(input.length, input.head.length)
    var steps = 1
    while (step(input, aux))
      steps += 1
    steps
  }

  override def solve2(input: Parsed): Solution2 = 0

}
