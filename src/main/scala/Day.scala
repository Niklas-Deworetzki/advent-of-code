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

  private val dayRegistry: mutable.Set[Day] = new mutable.HashSet[Day]()

  def getDay(year: Int, day: Int): Option[Day] =
    dayRegistry.find(d => d.day == day && d.year == year)

  def getInput(day: Day): String =
    cachedInputForDay(day)
      .orElse(downloadInputForDay(day))
      .get

  private def downloadInputForDay(day: Day): Try[String] = for {
    input <- onlineInputForDay(day)
    _ <- when(Main.doCacheInputs) {
      Try(Files.writeString(day.localPath, input, CREATE_NEW))
    }
  } yield input

  private def when(condition: Boolean)(action: => Try[Unit]): Try[Unit] =
    if (condition) action
    else Try(())

  private def fileForDay(day: Day): File =
    Main.inputCache.resolve(day.localPath).toFile

  private def cachedInputForDay(day: Day): Try[String] =
    Using(Source.fromFile(fileForDay(day)))(_.toString())

  private def onlineInputForDay(day: Day): Try[String] =
    Using(Source.fromURL(day.remoteURL))(_.toString())
}
