package aoc
package strategy

import aoc.strategy.Strategy.doTime

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

trait Strategy {
  type Preprocessed
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
    type Parsed

    def parse(input: Preprocessed): Parsed

    def solve1(input: Parsed): Solution1
    def solve2(input: Parsed): Solution2

    override def run(input: String): Unit = time("Total") {
      val parsed = parse(preprocess(input))
      println(time("Solution 1")(solve1(parsed)))
      println(time("Solution 2")(solve2(parsed)))
    }
  }

  trait Parallel extends Default {
    implicit private val executionContext: ExecutionContext = ExecutionContext.global

    override def run(input: String): Unit = time("Total") {
      val parsed = parse(preprocess(input))
      val solution2 = Future[Solution2](time("Solution 2")(solve2(parsed)))
      println(time("Solution 1")(solve1(parsed)))
      println(Await.result(solution2, Duration.Inf))
    }
  }


  trait Shared extends Strategy {
    type Parsed
    type Computed

    def parse(input: Preprocessed): Parsed
    def precompute(input: Parsed): Computed

    def solve1(input: Computed): Solution1
    def solve2(input: Computed): Solution2

    override def run(input: String): Unit = time("Total") {
      val parsed = parse(preprocess(input))
      val precomputed = time("Precompute")(precompute(parsed))
      println(time("Solution 1")(solve1(precomputed)))
      println(time("Solution 2")(solve2(precomputed)))
    }
  }

  trait TwoParsers extends Strategy {
    type Parsed1
    type Parsed2

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
}
