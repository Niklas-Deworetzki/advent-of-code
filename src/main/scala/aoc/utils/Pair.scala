package aoc.utils

import cats.kernel.Monoid

object Pair {

  given monoid: Monoid[(Int, Int)] = new Monoid[(Int, Int)] {
    override def empty: (Int, Int) = (0, 0)
    def combine(l: (Int, Int), r: (Int, Int)): (Int, Int) =
      (l._1 + r._1, l._2 + r._2)
  }

}
