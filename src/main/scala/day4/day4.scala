package day4

import scala.annotation.tailrec
import scala.io.Source

case class Position(x: Int, y: Int)

type Board = Map[Position, Int]

def part1(input: Iterable[String]): Int =
  val (boards, numbers) = parseInput(input)
  solve1(boards, numbers)

@tailrec
def solve1(boards: Iterable[Board], numbers: Iterable[Int]): Int =
  val currentNumber = numbers.head
  val updatedBoards = strikeNumbers(boards, currentNumber)
  val bingoBoard = getBingoBoard(updatedBoards)
  if (bingoBoard.isDefined)
    bingoBoard.get.values.sum * currentNumber
  else
    solve1(updatedBoards, numbers.tail)

def part2(input: List[String]): Int =
  val (boards, numbers) = parseInput(input)
  solve2(boards, numbers)

@tailrec
def solve2(boards: Iterable[Board], numbers: Iterable[Int], lastWinningBoard: Board = Map(), lastBingoNumber: Int = -1): Int =
  if (numbers.isEmpty || boards.isEmpty)
    lastWinningBoard.values.sum * lastBingoNumber
  else
    val currentNumber = numbers.head
    val updatedBoards = strikeNumbers(boards, currentNumber)
    val bingoBoard = getBingoBoard(updatedBoards)
    val boardsWithoutWinningBoards = updatedBoards.filter(!hasBingo(_))
    val (lastBingoBoard, newLastBingoNumber) = if bingoBoard.isDefined then (bingoBoard.get, currentNumber) else (lastWinningBoard, lastBingoNumber)
    solve2(boardsWithoutWinningBoards, numbers.tail, lastBingoBoard, newLastBingoNumber)

def getBingoBoard(boards: Iterable[Board]): Option[Board] =
  boards.find(hasBingo)

def hasBingo(board: Board): Boolean =
  val patterns = bingoPatterns()
  patterns.exists(_.forall(p => !board.contains(p)))

def strikeNumbers(boards: Iterable[Board], number: Int) =
  boards.map(_.filter(_._2 != number))

def bingoPatterns(): List[List[Position]] =
  List(
    (0 until 5).toList.map(Position(_, 0)),
    (0 until 5).toList.map(Position(_, 1)),
    (0 until 5).toList.map(Position(_, 2)),
    (0 until 5).toList.map(Position(_, 3)),
    (0 until 5).toList.map(Position(_, 4)),
    (0 until 5).toList.map(Position(0, _)),
    (0 until 5).toList.map(Position(1, _)),
    (0 until 5).toList.map(Position(2, _)),
    (0 until 5).toList.map(Position(3, _)),
    (0 until 5).toList.map(Position(4, _)),
  )

def parseInput(input: Iterable[String]): (Iterable[Board], Iterable[Int]) =
  val numbers = input.head.split(",").map(_.toInt)
  val boardInput = input.tail.tail
  val boards = parseBoards(boardInput)
  (boards, numbers)

def parseBoards(input: Iterable[String]): Iterable[Board] =
  input.filter(_.length > 1).grouped(5).map(makeBoard).toList

def makeBoard(input: Iterable[String]): Board =
  val boardLines = input.map(_.split(" ").filter(_.nonEmpty))
  Map.from(boardLines.zipWithIndex.flatMap(el => el._1.zipWithIndex.map(el2 => (Position(el._2, el2._2), el2._1.toInt))))


@main
def main(): Unit =
  val lines = Source.fromResource("day4.txt").getLines().toList

  println("Pt1: " + part1(lines))
  println("Pt2: " + part2(lines))


def example(): String =
  """|
7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
8  2 23  4 24
21  9 14 16  7
6 10  3 18  5
1 12 20 15 19

3 15  0  2 22
9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
2  0 12  3  7
     |""".stripMargin
