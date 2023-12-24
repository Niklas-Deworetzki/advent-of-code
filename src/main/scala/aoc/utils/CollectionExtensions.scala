package aoc.utils

import scala.collection.MapView
import scala.collection.mutable.ArrayBuffer

object CollectionExtensions {

  def zipWith[A, B, R](itA: Iterable[A], itB: Iterable[B], f: (A, B) => R): Iterable[R] = {
    val aIterator = itA.iterator
    val bIterator = itB.iterator
    val result = ArrayBuffer[R]()
    while (aIterator.hasNext && bIterator.hasNext) {
      result.addOne(f(aIterator.next(), bIterator.next()))
    }
    result
  }

  extension [A](iterable: Iterable[A])
    def countOccurrences(): MapView[A, Int] =
      iterable.groupBy(identity).view.mapValues(_.size)

  extension (string: String)
    def countOccurrences(): MapView[Char, Int] =
      string.groupBy(identity).view.mapValues(_.length)

  extension [K, V](map: MapView[K, V])
    def findKeyWhereValue(predicate: V => Boolean): Option[K] =
      map.find(entry => predicate(entry._2)).map(_._1)

  extension [A](elements: IndexedSeq[A]) {
    inline def pairwise(action: (A, A) => Unit): Unit = {
      for (x <- elements.indices) {
        for (y <- elements.indices.drop(x + 1)) {
          action(elements(x), elements(y))
        }
      }
    }

    inline def foldPairs[B](zero: B)(combinator: (B, A, A) => B): B = {
      var acc: B = zero
      pairwise { (x, y) =>
        acc = combinator(acc, x, y)
      }
      acc
    }

    inline def countPairsMatching(predicate: (A, A) => Boolean): Int =
      foldPairs(0) { (count, x, y) =>
        if (predicate(x, y)) count + 1
        else count
      }
  }
}
