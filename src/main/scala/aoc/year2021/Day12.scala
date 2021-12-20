package aoc.year2021

import aoc.Day
import aoc.strategy.Strategy

import scala.collection.mutable

object Day12 extends Day with Strategy.Default {
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
  override type Solution2 = Int

  def countPaths(graph: Parsed, currentPath: Set[String], currentNode: String, allowDuplicate: Boolean): Int = {
    if (currentNode == "end") 1
    else if (isTraversableOnce(currentNode) && currentPath.contains(currentNode)) {
      if (currentNode == "start" || !allowDuplicate) 0
      else graph(currentNode).map(countPaths(graph, currentPath + currentNode, _, false)).sum
    } else graph(currentNode).map(countPaths(graph, currentPath + currentNode, _, allowDuplicate)).sum
  }

  override def solve1(input: Parsed): Solution1 =
    countPaths(input, Set.empty, "start", false)

  override def solve2(input: Parsed): Solution2 =
    countPaths(input, Set.empty, "start", true)

  class Graph {
    private val edges: mutable.Map[String, mutable.Set[String]] = new mutable.HashMap()

    def addEdge(from: String, to: String): Unit = {
      if (!edges.contains(from)) edges.put(from, new mutable.HashSet())
      if (!edges.contains(to)) edges.put(to, new mutable.HashSet())

      edges(from).add(to)
      edges(to).add(from)
    }

    def apply(node: String): Iterator[String] = edges.get(node).getOrElse(Nil).iterator
  }
}