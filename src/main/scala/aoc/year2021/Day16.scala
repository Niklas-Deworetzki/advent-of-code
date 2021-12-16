package aoc.year2021

import aoc.Day
import aoc.strategy.Strategy

object Day16 extends Day with Strategy.Shared {
  override type Preprocessed = Iterable[Char]
  override type Parsed = List[Message]

  private def hexToBinary(hex: Char): String = {
    val binary = Integer.parseInt(hex.toString, 16).toBinaryString
    ("0" * (4 - binary.length)) + binary
  }

  override def preprocess(input: String): Preprocessed = input.flatMap(hexToBinary)

  private def seqToNumber(seq: Iterable[Char]): Long =
    seq.foldLeft(0) { (number, digit) =>
      number * 2 + (digit - '0')
    }

  private def consumeNumber(seq: Iterable[Char], previousNumber: Long = 0): (Long, Iterable[Char]) = {
    val (number, remaining) = seq.splitAt(5)
    val currentNumber = previousNumber * 16 + seqToNumber(number.tail)
    if (number.head == '1') consumeNumber(remaining, currentNumber)
    else (currentNumber, remaining)
  }

  override def parse(input: Preprocessed): Parsed = {
    if (input.forall(_ == '0')) Nil // Exit condition reached.
    else {
      val (version, remaining) = input.splitAt(3)
      val (typeid, data) = remaining.splitAt(3)
      seqToNumber(typeid) match {
        case 4 =>
          val (number, nextMessageData) = consumeNumber(data)
          Number(seqToNumber(version).toInt, number) :: parse(nextMessageData)

        case operator =>
          data.head match {
            case '0' =>
              val (subPacketsLength, packetData) = data.tail.splitAt(15)
              val (subPacketsData, nextMessageData) = packetData.splitAt(seqToNumber(subPacketsLength).toInt)

              Operator(seqToNumber(version).toInt, operator.toInt, parse(subPacketsData)) :: parse(nextMessageData)

            case '1' =>
              val (subPacketsCount, packetData) = data.tail.splitAt(11)
              val followingMessages = parse(packetData)

              val (messageBody, nextMessages) = followingMessages.splitAt(seqToNumber(subPacketsCount).toInt)
              Operator(seqToNumber(version).toInt, operator.toInt, messageBody) :: nextMessages
          }
      }
    }
  }


  sealed abstract class Message(version: Int) {
    def typeId: Int
  }
  case class Number(version: Int, value: Long) extends Message(version) {
    override def typeId: Int = 4
    override def toString: String = s"Number(version=$version, type=$typeId, value=$value)"
  }
  case class Operator(version: Int, override val typeId: Int, data: List[Message]) extends Message(version) {
    override def toString: String = s"Operator(version=$version, type=$typeId, value=${data.mkString("[", ", ", "]")})"
  }

  override type Solution1 = Long
  override type Solution2 = Long

  def sumVersionNumbers: Message => Long = {
    case number: Number => number.version
    case operator: Operator => operator.version + operator.data.map(sumVersionNumbers).sum
  }

  def eval: Message => Long = {
    case number: Number => number.value
    case Operator(_, 0, operands) => operands.map(eval).sum
    case Operator(_, 1, operands) => operands.map(eval).product
    case Operator(_, 2, operands) => operands.map(eval).min
    case Operator(_, 3, operands) => operands.map(eval).max
    case Operator(_, 5, List(lhs, rhs)) => if (eval(lhs) > eval(rhs)) 1 else 0
    case Operator(_, 6, List(lhs, rhs)) => if (eval(lhs) < eval(rhs)) 1 else 0
    case Operator(_, 7, List(lhs, rhs)) => if (eval(lhs) == eval(rhs)) 1 else 0
  }

  override def solve1(input: Parsed): Solution1 =
    sumVersionNumbers(input.head)

  override def solve2(input: Parsed): Solution2 =
    eval(input.head)
}