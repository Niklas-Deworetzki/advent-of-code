package aoc.year2023

import aoc.Day
import aoc.strategy.Strategy

object Day04 extends Day with Strategy.Default {
  override type Preprocessed = Iterator[String]
  override type Parsed = IndexedSeq[Card]

  override def preprocess(input: String): Preprocessed = input.linesIterator
  override def parse(input: Preprocessed): Parsed = input.map(parseCard).toIndexedSeq

  override type Solution1 = Long
  override type Solution2 = Long

  override def solve1(input: Parsed): Solution1 =
    input.map(_.score).sum

  override def solve2(input: Parsed): Solution2 = {
    val amountOfCards = Array.fill(input.size)(1)
    for (index <- amountOfCards.indices) {
      val currentCard = input(index)
      val instances = amountOfCards(index)

      val cardsWon = (index + 1) to (index + currentCard.winningNumbers.size)
      for (cardWon <- cardsWon)
        amountOfCards(cardWon) += instances
    }
    amountOfCards.sum
  }

  private val REGEX_NUMBERS = "[0-9]+".r

  private def parseCard(line: String): Card = {
    val beginOfWinningSection = line.indexOf(':')
    val beginOfCardSection = line.indexOf('|')

    val idString = REGEX_NUMBERS.findFirstIn(line).get
    val winningSection = line.substring(beginOfWinningSection, beginOfCardSection)
    val cardSection = line.substring(beginOfCardSection)

    Card(
      idString.toInt,
      parseNumbers(winningSection),
      parseNumbers(cardSection)
    )
  }

  private def parseNumbers(numberString: String): Iterable[Int] =
    REGEX_NUMBERS.findAllIn(numberString)
      .map(_.toInt)
      .toList

  case class Card(private val id: Int, cardWinningNumbers: Iterable[Int], private val cardNumbers: Iterable[Int]) {
    val winningNumbers: Set[Int] =
      cardWinningNumbers.toSet intersect cardNumbers.toSet

    def score: Long =
      if (cardWinningNumbers.isEmpty) 0
      else Math.pow(2, winningNumbers.size - 1).toLong
  }
}
