package aoc
package strategy

import aoc.strategy.Strategy.doTime

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

trait Strategy {
  type Preprocessed
  type Parsed1
  type Parsed2
  type Solution1
  type Solution2

  val enableTimingOutput: Boolean = false

  def time[A](tag: String)(action: => A): A = if (enableTimingOutput) doTime(tag)(action) else action

  def run(input: String): Unit

  def preprocess(input: String): Preprocessed

}

object Strategy {

  def doTime[A](tag: String)(action: => A): A = {
    val startTime: Long = System.currentTimeMillis()
    val result = action
    println(s"$tag: ${System.currentTimeMillis() - startTime}ms")
    result
  }

  trait NoPreprocessing extends Strategy {
    override type Preprocessed = String

    def preprocess(input: String): String = input
  }

  trait Default extends Strategy {
    def parse1(input: Preprocessed): Parsed1
    def solve1(input: Parsed1): Solution1

    def parse2(input: Preprocessed): Parsed2
    def solve2(input: Parsed2): Solution2

    override def run(input: String): Unit = time("Total") {
      val preprocessed = preprocess(input)
      println(time("Solution 1")(solve1(parse1(preprocessed))))
      println(time("Solution 2")(solve2(parse2(preprocessed))))
    }
  }

  trait Shared extends Strategy {
    type Parsed
    override type Parsed1 = Parsed
    override type Parsed2 = Parsed

    def parse(input: Preprocessed): Parsed

    def solve1(input: Parsed): Solution1
    def solve2(input: Parsed): Solution2

    override def run(input: String): Unit = time("Total") {
      val parsed = parse(preprocess(input))
      println(time("Solution 1")(solve1(parsed)))
      println(time("Solution 2")(solve2(parsed)))
    }
  }

  trait Incrementing extends Strategy {
    override type Parsed2 = Nothing

    def parse1(input: Preprocessed): Parsed1
    def solve1(input: Parsed1): Solution1
    def solve2(solution1: Solution1): Solution2

    override def run(input: String): Unit = time("Total") {
      val preprocessed = preprocess(input)
      val solution1 = time("Solution 1")(solve1(parse1(preprocessed)))
      println(solution1)
      val solution2 = time("Solution 1")(solve2(solution1))
      println(solution2)
    }
  }

  trait Parallel extends Default {
    implicit private val executionContext: ExecutionContext = ExecutionContext.global

    override def run(input: String): Unit = time("Total") {
      val preprocessed = preprocess(input)
      val solution2 = Future[Solution2](time("Solution 2")(solve2(parse2(preprocessed))))
      println(time("Solution 1")(solve1(parse1(preprocessed))))
      println(Await.result(solution2, Duration.Inf))
    }
  }

  trait ParallelShared extends Shared {
    implicit private val executionContext: ExecutionContext = ExecutionContext.global

    override def run(input: String): Unit = time("Total") {
      val parsed = parse(preprocess(input))
      val solution2 = Future[Solution2](time("Solution 2")(solve2(parsed)))
      println(time("Solution 1")(solve1(parsed)))
      println(Await.result(solution2, Duration.Inf))
    }
  }
}
