package day13

import scala.annotation.tailrec
import scala.io.Source

type Instructions = List[Fold]
type Paper = Set[Position]

case class Position(x: Int, y: Int)
case class Fold(direction: Direction, position: Int)

enum Direction:
  case X, Y


def foldPaper(dots: Paper, fold: Fold): Paper = fold.direction match {
  case Direction.X => foldX(dots, fold.position)
  case Direction.Y => foldY(dots, fold.position)
}

def foldX(dots: Paper, foldAt: Int): Paper =
  val xMax = dots.maxBy(_.x).x
  val yMax = dots.maxBy(_.y).y
  val dotsToKeep = dots.filter(_.x < foldAt)
  val dotsToMove = dots.filter(_.x > foldAt)
  val movedDots = dotsToMove.map(p => Position(foldAt - (p.x - foldAt), p.y))
  dotsToKeep.union(movedDots)

def foldY(dots: Paper, foldAt: Int): Paper =
  val xMax = dots.maxBy(_.x).x
  val yMax = dots.maxBy(_.y).y
  val dotsToKeep = dots.filter(_.y < foldAt)
  val dotsToMove = dots.filter(_.y > foldAt)
  val movedDots = dotsToMove.map(p => Position(p.x, foldAt - (p.y - foldAt)))
  dotsToKeep.union(movedDots)

@tailrec
def parseInput(lines: List[String], dots: Paper = Set(), folds: Instructions = List()): (Paper, Instructions) = lines match {
  case List() => (dots, folds.reverse)
  case _ =>
    val (updatedDots, updatedFolds) = getInput(lines.head, dots, folds)
    parseInput(lines.tail, updatedDots, updatedFolds)
}

def getInput(line: String, dots: Paper, folds: Instructions): (Paper, Instructions) = line match {
  case s"$x,$y"             => (dots + Position(x.toInt, y.toInt), folds)
  case s"fold along y=$pos" => (dots, Fold(Direction.Y, pos.toInt) :: folds)
  case s"fold along x=$pos" => (dots, Fold(Direction.X, pos.toInt) :: folds)
  case _                    => throw RuntimeException(s"Cant parse $line")
}

@main
def main(): Unit =
  val lines = Source.fromResource("day13.txt").getLines().filterNot(_.isBlank).toList
  //val lines = example.split("\n").toList
  val (dots, folds) = parseInput(lines)

  println("Pt1: " + foldPaper(dots, folds.head).size)
  val dots2 = folds.foldLeft(dots)(foldPaper)
  print(dots2)

def print(dots: Paper): Unit =
  val xMax = dots.maxBy(_.x).x
  val yMax = dots.maxBy(_.y).y
  (0 to yMax).foreach(y => println((0 to xMax).map(x => if dots.contains(Position(x, y)) then "#" else " ").mkString))

val example =
  """
    |6,10
    |0,14
    |9,10
    |0,3
    |10,4
    |4,11
    |6,0
    |6,12
    |4,1
    |0,13
    |10,12
    |3,4
    |3,0
    |8,4
    |1,10
    |2,14
    |8,10
    |9,0
    |
    |fold along y=7
    |fold along x=5
    |""".stripMargin

