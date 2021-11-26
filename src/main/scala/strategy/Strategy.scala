package aoc
package strategy

import scala.concurrent.{ExecutionContext, Future}

trait Strategy extends Runnable {
  type Preprocessed
  type Parsed1
  type Parsed2
  type Solution1
  type Solution2

  def run(input: String): Unit

  def preprocess(input: String): Preprocessed

}

object Strategy {

  trait NoPreprocessing extends Strategy {
    override type Preprocessed = String

    def preprocess(input: String): String = input
  }

  trait Default extends Strategy {
    def parse1(input: Preprocessed): Parsed1
    def solve1(input: Parsed1): Solution1

    def parse2(input: Preprocessed): Parsed2
    def solve2(input: Parsed2): Solution2

    override def run(input: String): Unit = {
      val preprocessed = preprocess(input)
      println(solve1(parse1(preprocessed)))
      println(solve2(parse2(preprocessed)))
    }
  }


  trait Incrementing extends Strategy {
    override type Parsed2 = Nothing

    def parse1(input: Preprocessed): Parsed1
    def solve1(input: Parsed1): Solution1
    def solve2(solution1: Solution1): Solution2

    override def run(input: String): Unit = {
      val preprocessed = preprocess(input)
      val solution1 = solve1(parse1(preprocessed))
      println(solution1)
      val solution2 = solve2(solution1)
      println(solution2)
    }
  }

  trait Parallel extends Strategy {
    def parse1(input: Preprocessed): Parsed1
    def solve1(input: Parsed1): Solution1

    def parse2(input: Preprocessed): Parsed2
    def solve2(input: Parsed2): Solution2

    implicit private val executionContext: ExecutionContext = ExecutionContext.global

    override def run(input: String): Unit = {
      val preprocessed = preprocess(input)
      val solution2 = Future[Solution2](solve2(parse2(preprocessed)))
      println(solve1(parse1(preprocessed)))
      solution2.onComplete(_.foreach(println))
    }
  }

}