package aoc.year2021

import aoc.Day
import aoc.strategy.Strategy

import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable

object Day15 extends Day with Strategy.Default {
  override type Preprocessed = Iterator[String]
  override type Parsed = IndexedSeq[IndexedSeq[Int]]

  override def preprocess(input: String): Preprocessed = input.linesIterator

  override def parse(input: Preprocessed): Parsed = {
    for (line <- input)
      yield line.map(_ - '0')
  }.toIndexedSeq


  override type Solution1 = Long
  override type Solution2 = Long

  private def isDefined(graph: Parsed)(x: Int, y: Int): Boolean =
    graph.isDefinedAt(y) && graph(y).isDefinedAt(x)

  private def valueOf(graph: Parsed, default: Int = Int.MaxValue): ((Int, Int)) => Int = {
    case (x, y) => if (isDefined(graph)(x, y)) graph(y)(x) else default
  }

  private def neighbours: ((Int, Int)) => IndexedSeq[(Int, Int)] = {
    case (x, y) => for (xd <- -1 to 1; yd <- -1 to 1; if xd == 0 ^ yd == 0)
      yield (x + xd, y + yd)
  }

  private def shortestPath(graph: Parsed): Int = {
    given priorityOrder: Ordering[(Int, (Int, Int))] = Ordering.by[(Int, (Int, Int)), Int](_._1).reverse

    val distances: Array[Array[Int]] = Array.fill(graph.size, graph(0).size)(Int.MaxValue)
    val visited: Array[Array[Boolean]] = Array.ofDim(graph.size, graph(0).size)
    val toVisit: mutable.PriorityQueue[(Int, (Int, Int))] = new mutable.PriorityQueue()

    distances(0)(0) = 0
    toVisit.addOne((0, (0, 0)))
    while (toVisit.nonEmpty) {
      val (_, (x, y)) = toVisit.dequeue()
      visited(y)(x) = true

      for ((nx, ny) <- neighbours((x, y)); if isDefined(graph)(nx, ny) && !visited(ny)(nx)) {
        val previousDistance = distances(ny)(nx)
        val currentDistance = distances(y)(x) + graph(ny)(nx)

        if (currentDistance < previousDistance) {
          distances(ny)(nx) = currentDistance
          toVisit.enqueue((currentDistance, (nx, ny)))
        }
      }
    }

    distances.last.last
  }

  override def solve1(input: Parsed): Solution1 =
    shortestPath(input)

  private def expandMap(original: Parsed): Parsed = {
    val buffer: Array[Array[Int]] = Array.ofDim(original.size * 5, original(0).size * 5)

    def copyOrCalc(x: Int, y: Int): Unit = {
      if (isDefined(original)(x, y)) buffer(y)(x) = original(y)(x)
      else {
        val neighbours = List(
          (x, y - original.size),
          (x - original(0).size, y),
        ).flatMap { case (x, y) =>
          if (buffer.isDefinedAt(y) && buffer(y).isDefinedAt(x)) Some(buffer(y)(x))
          else None
        }

        buffer(y)(x) = (neighbours.max % 9) + 1
      }
    }

    for (y <- buffer.indices; x <- buffer(y).indices)
      copyOrCalc(x, y)
    buffer.map(_.toIndexedSeq).toIndexedSeq
  }

  override def solve2(input: Parsed): Solution2 =
    shortestPath(expandMap(input))

}