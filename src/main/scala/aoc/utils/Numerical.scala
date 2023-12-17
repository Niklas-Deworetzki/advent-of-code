package aoc.utils

import scala.annotation.tailrec

object Numerical {

  extension (xs: Iterable[Long])
    def lcm: Long =
      xs.reduce(Numerical.lcm)

  def lcm(x: Long, y: Long): Long =
    x * y / gcd(x, y)

  @tailrec
  def gcd(x: Long, y: Long): Long = y match {
    case 0 => x
    case _ => gcd(y, x % y)
  }
}
