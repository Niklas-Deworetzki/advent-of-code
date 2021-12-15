package aoc

import aoc.strategy.Strategy

import java.nio.file.{Path, Paths}
import scala.util.{Failure, Success}

object Main {
  val remote: String = "https://adventofcode.com/%d/day/%d"

  val doCacheInputs: Boolean = true
  val inputCache: Path = Paths.get(".", "inputs")

  private val QualifiedDay = "([0-9][0-9][0-9][0-9]).([0-9]+)".r
  private val SingleDay = "([0-9]+)".r


  def main(args: Array[String]): Unit = Strategy.doTime("Total") {
    args.foreach {
      case QualifiedDay(year, day) =>
        execute(year.toInt, day.toInt)
      case SingleDay(day) =>
        execute(java.time.Year.now().getValue, day.toInt)
      case _ =>
        println("Please enter a day like 17 or 2020/13")
    }
  }

  private final def execute(year: Int, day: Int): Unit =
    Day(year, day) match {
      case Success(solution) => solution.run()
      case Failure(exception) => println(exception.getMessage)
    }
}
