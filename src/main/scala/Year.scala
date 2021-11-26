package aoc

import java.util.Objects

case class Year(year: Int, days: Day*) {
  days.foreach(_.year = year)

  def getDay(day: Int): Option[Day] =
    days.lift(day - 1)
}
