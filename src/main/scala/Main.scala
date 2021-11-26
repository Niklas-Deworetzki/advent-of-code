package aoc

import java.nio.file.Path
import java.time.Year

object Main {
  val remote: String = "https://adventofcode.com/%d/day/%d"

  val doCacheInputs: Boolean = true
  val inputCache: Path = Path.of(".", "aoc", "inputs")

  private val QualifiedDay = "([0-9][0-9][0-9][0-9]).([0-9]+)".r
  private val SingleDay = "([0-9]+)".r

  def main(args: Array[String]): Unit = args.foreach {
    case QualifiedDay(year, day) =>
      execute(year.toInt, day.toInt)
    case SingleDay(day) =>
      execute(Year.now().getValue, day.toInt)
    case _ =>
      println("Please enter a day like 17 or 2020/13")
  }

  private final def execute(year: Int, day: Int): Unit =
    Day.getDay(year, day).get.run()

}
