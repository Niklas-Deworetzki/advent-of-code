package aoc.utils

object NumberExtensions {

  extension (n: Int) {
    def sumUpTo: Int =
      n * (n + 1) / 2

    inline def wrapTo(bound: Int): Int =
      (n - 1) % bound + 1
  }
}
