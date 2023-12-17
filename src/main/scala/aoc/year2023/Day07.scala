package aoc.year2023

import aoc.Day
import aoc.strategy.Strategy

import aoc.utils.CollectionExtensions.*

object Day07 extends Day with Strategy.Default {
  override type Preprocessed = Iterator[String]
  override type Parsed = List[Hand]

  private val DEFAULT_CARD_STRENGTHS = "AKQJT98765432"
  private val JOKER_CARD_STRENGTHS = "AKQT98765432J"

  override val enableTimingOutput: Boolean = true

  override def preprocess(input: String): Preprocessed = input.linesIterator
  override def parse(input: Preprocessed): Parsed =
    input.map { line =>
      val parts = line.split(' ')
      Hand(parts.head, parts.last.toInt)
    }.toList


  override type Solution1 = Int
  override type Solution2 = Int

  extension (rankedHands: Iterable[Hand])
    private def calculateWinnings(): Int =
      rankedHands.zipWithIndex
        .map { case (hand, rank) =>
          // println(s"${rank + 1}: ${hand.cards}")
          hand.bid * (rank + 1)
        }
        .sum

  override def solve1(input: Parsed): Solution1 =
    input
      .sorted(OrderByScore(_.score).orElse(OrderByStrength(DEFAULT_CARD_STRENGTHS)))
      .calculateWinnings()

  override def solve2(input: Parsed): Solution2 =
    input
      .sorted(OrderByScore(_.scoreWithJokers).orElse(OrderByStrength(JOKER_CARD_STRENGTHS)))
      .calculateWinnings()

  private case class OrderByStrength(strengths: String) extends Ordering[Hand] {
    private def compareStrengths(x: Char, y: Char): Int =
      strengths.indexOf(y) - strengths.indexOf(x)

    override def compare(x: Hand, y: Hand): Int =
      zipWith(x.cards, y.cards, compareStrengths)
        .find(_ != 0)
        .getOrElse(0)
  }

  private case class OrderByScore(scoreFunction: Hand => Score) extends Ordering[Hand] {
    override def compare(xHand: Hand, yHand: Hand): Int = {
      val xScore = scoreFunction(xHand)
      val yScore = scoreFunction(yHand)
      yScore.ordinal - xScore.ordinal
    }
  }

  case class Hand(cards: String, bid: Int) {
    private val occurringCards = cards.countOccurrences()

    val score: Score =
      genericScoreFunction(occurringCards.toList, 0)

    val scoreWithJokers: Score = {
      val amountOfJokers = occurringCards.getOrElse('J', 0)
      val cardsWithoutJokers = occurringCards
        .filterKeys(_ != 'J')
        .toList

      genericScoreFunction(cardsWithoutJokers, amountOfJokers)
    }

    private def genericScoreFunction(
      occurringCards: List[(Char, Int)],
      amountOfJokers: Int
    ): Score = {
      val sortedCards = occurringCards
        .sortBy(_._2)
        .reverse

      val mostCommonCard = sortedCards.headOption.map(_._2).getOrElse(0)
      val secondMostCommonCard = sortedCards.lift(1).map(_._2).getOrElse(0)
      mostCommonCard + amountOfJokers match {
        case x if x >= 5 =>
          Score.FiveOfKind
        case 4 =>
          Score.FourOfKind
        case 3 if secondMostCommonCard == 2 =>
          Score.FullHouse
        case 3 =>
          Score.ThreeOfKind
        case 2 if secondMostCommonCard == 2 =>
          Score.TwoPairs
        case 2 =>
          Score.OnePair
        case _ =>
          Score.HighestCard
      }
    }
  }

  enum Score {
    case FiveOfKind
    case FourOfKind
    case FullHouse
    case ThreeOfKind
    case TwoPairs
    case OnePair
    case HighestCard
  }
}
