package aoc.year2023

import aoc.Day
import aoc.strategy.Strategy
import aoc.utils.Numerical.*

import scala.collection.mutable

object Day08 extends Day with Strategy.Default {
  override type Preprocessed = Iterator[String]
  override type Parsed = Document

  override def preprocess(input: String): Preprocessed = input.linesIterator
  override def parse(input: Preprocessed): Parsed = {
    val path = input.next()
    input.next() // Skip empty line.

    val network = mutable.Map[String, Node]()
    for (line <- input) {
      val id = line.substring(0, 3)
      val l = line.substring(7, 10)
      val r = line.substring(12, 15)
      network.put(id, Node(l, r))
    }
    Document(path, network.toMap)
  }

  override type Solution1 = Long
  override type Solution2 = Long

  override def solve1(input: Parsed): Solution1 =
    countIterations(input, "AAA", _ == "ZZZ")

  override def solve2(input: Parsed): Solution2 = {
    val startNodes = input.network.keys.filter(_.last == 'A')
    startNodes.map { node => 
      countIterations(input, node, _.last == 'Z')
    }.lcm
  }

  private def countIterations(
    document: Document,
    startPoint: String,
    isFinish: String => Boolean
  ): Long = {
    val path = Iterator.continually(document.path).flatten

    var current = startPoint
    var count = 0L
    while (!isFinish(current)) {
      count += 1
      current = document.network(current)(path.next())
    }
    count
  }

  case class Document(path: String, network: Map[String, Node])

  case class Node(l: String, r: String) {
    def apply(instruction: Char): String =
      if (instruction == 'L') l else r
  }
}
