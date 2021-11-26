package aoc

import strategy.Strategy

import aoc.Day.{dayRegistry, getInput}

import java.io.File
import java.net.URL
import java.nio.file.{Files, Path}
import java.nio.file.StandardOpenOption.*
import scala.collection.mutable
import scala.io.Source
import scala.util.{Try, Using}

case class Day(year: Int, day: Int, strategy: Strategy) extends Runnable {
  dayRegistry.add(this)

  override def run(): Unit = {
    println(s"Solving $year/$day. Your solution can be submitted here: " + remoteURL)
    strategy.run(getInput(this))
  }

  private val localPath: Path =
    Path.of(year.toString, day.toString)

  private val remoteURL: URL =
    new URL(Main.remote.format(year, day))
}

object Day {
  def getInput(day: Day): String =
    cachedInputForDay(day).get

  private def fileForDay(day: Day): File =
    Main.inputCache.resolve(day.localPath).toFile

  private def cachedInputForDay(day: Day): Try[String] =
    Using(Source.fromFile(fileForDay(day)))(_.mkString)
}
