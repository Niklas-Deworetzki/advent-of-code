package aoc

import year2015.*

import java.nio.file.{Path, Paths}

object Main {
  val remote: String = "https://adventofcode.com/%d/day/%d"

  val doCacheInputs: Boolean = true
  val inputCache: Path = Paths.get(".", "inputs")

  private val QualifiedDay = "([0-9][0-9][0-9][0-9]).([0-9]+)".r
  private val SingleDay = "([0-9]+)".r

  val years: List[Year] = List(
    Year(2015, Day01),
  )

  def main(args: Array[String]): Unit = args.foreach {
    case QualifiedDay(year, day) =>
      execute(year.toInt, day.toInt)
    case SingleDay(day) =>
      execute(java.time.Year.now().getValue, day.toInt)
    case _ =>
      println("Please enter a day like 17 or 2020/13")
  }

  private final def execute(year: Int, day: Int): Unit =
    years.find(_.year == year).flatMap(_.getDay(day)) match {
      case Some(solution) => solution.run()
      case None => println(s"Year $year does not have a solution for day $day.")
    }
}
