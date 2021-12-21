package aoc.year2021

import aoc.Day
import aoc.strategy.Strategy
import aoc.utils.NumberExtensions.*
import cats.kernel.Monoid

import scala.collection.mutable

object Day21 extends Day with Strategy.Default with Strategy.NoPreprocessing {
  override type Parsed = (Int, Int)

  private val InputPattern = "Player 1 starting position: (\\d+)\nPlayer 2 starting position: (\\d+)\n?".r

  override def parse(input: Preprocessed): Parsed = input match {
    case InputPattern(a, b) => (a.toInt, b.toInt)
  }

  override type Solution1 = Long
  override type Solution2 = Long

  // Part 1

  class DeterministicDie(var state: Int = 1) {
    private var rolls: Int = 0

    def next(): Int = {
      rolls += 3
      val result = state + (state + 1).wrapTo(100) + (state + 2).wrapTo(100)
      state = (state + 3).wrapTo(100)
      result
    }

    def getRolls(): Int = rolls
  }

  override def solve1(input: Parsed): Solution1 = {
    val die = new DeterministicDie()
    var p1Points = 0
    var p1 = input._1
    var p2Points = 0
    var p2 = input._2
    var isPlayer1Turn = true

    while (p1Points < 1000 && p2Points < 1000) {
      val rolled = die.next()
      if (isPlayer1Turn) {
        p1 = (p1 + rolled).wrapTo(10)
        p1Points += p1
      } else {
        p2 = (p2 + rolled).wrapTo(10)
        p2Points += p2
      }
      isPlayer1Turn = !isPlayer1Turn
    }

    die.getRolls() * Math.min(p1Points, p2Points)
  }

  // Part 2

  extension (pair: (Long, Long)) {
    def +(other: (Long, Long)): (Long, Long) =
      (pair._1 + other._1, pair._2 + other._2)

    def *(scalar: Int): (Long, Long) =
      (pair._1 * scalar, pair._2 * scalar)
  }

  case class Gamestate(p1: Int, p2: Int, isP1Turn: Boolean = true, p1Points: Int = 0, p2Points: Int = 0) {
    def roll(n: Int): Gamestate =
      if (isP1Turn) {
        val p1_ = (p1 + n).wrapTo(10)
        new Gamestate(p1_, p2, false, p1Points + p1_, p2Points)
      } else {
        val p2_ = (p2 + n).wrapTo(10)
        new Gamestate(p1, p2_, true, p1Points, p2Points + p2_)
      }

    def isWinningConfiguration: Boolean =
      p1Points >= 21 || p2Points >= 21

    def getWinners: (Long, Long) =
      if (p1Points >= 21) (1, 0) else if (p2Points >= 21) (0, 1) else (0, 0)
  }

  def winnersFor(gamestate: Gamestate, memory: mutable.Map[Gamestate, (Long, Long)] = mutable.HashMap.empty): (Long, Long) =
    memory.get(gamestate) match {
      case Some(result) => result
      case None =>
        val result = if (gamestate.isWinningConfiguration) gamestate.getWinners
        else winnersFor(gamestate.roll(3), memory) +
          winnersFor(gamestate.roll(4), memory) * 3 +
          winnersFor(gamestate.roll(5), memory) * 6 +
          winnersFor(gamestate.roll(6), memory) * 7 +
          winnersFor(gamestate.roll(7), memory) * 6 +
          winnersFor(gamestate.roll(8), memory) * 3 +
          winnersFor(gamestate.roll(9), memory)

        memory.put(gamestate, result)
        result
    }


  override def solve2(input: Parsed): Solution2 = {
    val (p1Wins, p2Wins) = winnersFor(new Gamestate(input._1, input._2))
    Math.max(p1Wins, p2Wins)
  }
}