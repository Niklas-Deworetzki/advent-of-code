package aoc

import strategy.Strategy

import java.io.File
import java.net.URL
import java.nio.file.{Files, Path}
import java.nio.file.StandardOpenOption.*
import scala.collection.mutable
import scala.io.Source
import scala.util.{Failure, Success, Try, Using}

abstract class Day(val day: Int) extends Runnable with Strategy {
  var year: Int = 0

  def run(): Unit = Day.getInput(this) match {
    case Success(input) =>
      println(s"Solving $year/$day. Your solution can be submitted here: " + remoteURL)
      this.run(input)
    case _: Failure[_] =>
      println(s"No input found for $year/$day. Download it here: " + remoteURL + "/input")
  }

  private def localPath: Path =
    Path.of(year.toString, day.toString + ".txt")

  private def remoteURL: String =
    Main.remote.format(year, day)
}

object Day {
  def getInput(day: Day): Try[String] =
    cachedInputForDay(day)

  private def fileForDay(day: Day): File =
    Main.inputCache.resolve(day.localPath).toFile

  private def cachedInputForDay(day: Day): Try[String] =
    Using(Source.fromFile(fileForDay(day)))(_.mkString)
}
