package aoc.year2021

import aoc.Day
import aoc.strategy.Strategy

import scala.collection.mutable

object Day12 extends Day with Strategy.Shared {
  override type Preprocessed = Iterator[(String, String)]
  override type Parsed = Graph

  private val Line = "([A-Za-z]+)-([A-Za-z]+)".r

  override def preprocess(input: String): Preprocessed = input.linesIterator.map {
    case Line(from, to) => (from, to)
  }

  override def parse(input: Preprocessed): Parsed = {
    val graph = new Graph
    input.foreach(Function.tupled(graph.addEdge))
    graph
  }

  def isTraversableOnce(nodeId: String): Boolean =
    nodeId.head.isLower

  override type Solution1 = Int
  override type Solution2 = Long

  def findPaths(graph: Parsed, currentPath: List[String], currentNode: String, allowDuplicate: Boolean): Iterable[List[String]] = {
    if (currentNode == "end") Some(currentNode :: currentPath)
    else if (isTraversableOnce(currentNode) && currentPath.contains(currentNode)) {
      if (currentNode == "start" || !allowDuplicate) Nil
      else {
        val nextPath = currentNode :: currentPath
        graph(currentNode).map(findPaths(graph, nextPath, _, false)).flatten
      }
    }
    else {
      val nextPath = currentNode :: currentPath
      graph(currentNode).map(findPaths(graph, nextPath, _, allowDuplicate)).flatten
    }
  }

  override def solve1(input: Parsed): Solution1 =
    findPaths(input, Nil, "start", false).size

  override def solve2(input: Parsed): Solution2 =
    findPaths(input, Nil, "start", true).size

  class Graph {
    private val edges: mutable.Map[String, mutable.Set[String]] = new mutable.HashMap()

    def addEdge(from: String, to: String): Unit = {
      if (!edges.contains(from)) edges.put(from, new mutable.HashSet())
      if (!edges.contains(to)) edges.put(to, new mutable.HashSet())

      edges(from).add(to)
      edges(to).add(from)
    }

    def apply(node: String): Iterable[String] = edges.get(node).getOrElse(Nil)
  }
}