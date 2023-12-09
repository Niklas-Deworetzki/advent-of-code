package aoc

import aoc.Day.{getInfo, register}
import aoc.strategy.Strategy

import java.io.File
import java.net.URL
import java.nio.file.StandardOpenOption.*
import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.io.Source
import scala.util.{Failure, Success, Try, Using}

abstract class Day extends Runnable with Strategy {
  val (year, day) = getInfo(this)
  register(this)

  def run(): Unit = Day.getInput(this) match {
    case Success(input) =>
      println(s"Solving $year/$day. Your solution can be submitted here: $remoteURL")
      this.run(input)
    case _: Failure[_] =>
      println(s"No input found at $localPath. Download it here: $remoteURL/input")
  }

  private def localPath: Path =
    Path.of(year.toString, day.toString + ".txt")

  private def remoteURL: String =
    Main.remote.format(year, day)

  def main(args: Array[String]): Unit = run()
}

object Day {
  private val challengeRegistry: mutable.Map[Int, Array[Day]] = new mutable.HashMap()

  private def register(day: Day): Unit = {
    if (!challengeRegistry.contains(day.year)) {
      challengeRegistry.put(day.year, new Array[Day](25))
    }
    challengeRegistry(day.year).update(day.day - 1, day)
  }


  private val ExtractInfo = """aoc.year(\d+).Day(\d+).*\$""".r

  private def getInfo(day: Day): (Int, Int) = day.getClass.getCanonicalName match {
    case ExtractInfo(year, date) => (year.toInt, date.toInt)
    case _ => (0, 0)
  }


  private def loadDay(year: Int, day: Int): Try[Unit] = Try {
    Class.forName(String.format("aoc.year%d.Day%02d$", year, day))
  }

  private def findDay(year: Int, day: Int): Try[Day] = Try {
    val fetchDay = for {
      daysInYear <- challengeRegistry.get(year)
      dayInstance <- daysInYear.lift(day - 1)
      if dayInstance != null
    } yield dayInstance
    fetchDay.getOrElse(throw new NoSuchElementException(s"Day $year/$day is not registered."))
  }

  def apply(year: Int, day: Int): Try[Day] =
    loadDay(year, day).flatMap(_ => findDay(year, day))

  def getInput(day: Day): Try[String] =
    cachedInputForDay(day)

  private def fileForDay(day: Day): File =
    Main.inputCache.resolve(day.localPath).toFile

  private def cachedInputForDay(day: Day): Try[String] =
    Using(Source.fromFile(fileForDay(day)))(_.mkString)
}
