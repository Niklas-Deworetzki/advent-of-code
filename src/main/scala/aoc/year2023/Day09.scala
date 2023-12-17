package aoc.year2023

import aoc.Day
import aoc.strategy.Strategy

import scala.collection.mutable

object Day09 extends Day with Strategy.Default {
  override type Preprocessed = Iterator[String]
  override type Parsed = List[IndexedSeq[Long]]

  override def preprocess(input: String): Preprocessed = input.linesIterator
  override def parse(input: Preprocessed): Parsed =
    input.map(_.split(' ').map(_.toLong).toIndexedSeq).toList

  override type Solution1 = Long
  override type Solution2 = Long

  override def solve1(input: Parsed): Solution1 =
    input.map(extrapolateFw).sum

  override def solve2(input: Parsed): Solution2 =
    input.map(extrapolateBw).sum

  private def extrapolateFw(sequence: IndexedSeq[Long]): Long =
    if (sequence.forall(_ == 0)) 0
    else {
      val differences = computeDifferences(sequence)
      sequence.last + extrapolateFw(differences)
    }

  private def extrapolateBw(sequence: IndexedSeq[Long]): Long =
    if (sequence.forall(_ == 0)) 0
    else {
      val differences = computeDifferences(sequence)
      sequence.head - extrapolateBw(differences)
    }

  private def computeDifferences(sequence: IndexedSeq[Long]): IndexedSeq[Long] = {
    val differences = mutable.ArrayBuffer[Long]()
    for (index <- sequence.indices.init) {
      differences.addOne(sequence(index + 1) - sequence(index))
    }
    differences.toIndexedSeq
  }
}
