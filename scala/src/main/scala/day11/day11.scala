package day11

import scala.annotation.tailrec
import scala.io.Source

case class Position(x: Int, y: Int)

type Energy = Int
type Field = Map[Position, Energy]

@tailrec
def solve(field: Field, iterations: Int, currIteration: Int = 0, accumulatedFlashes: Int = 0): Int =
  if iterations == currIteration then
    accumulatedFlashes
  else
    val (updatedField, newFlashes) = nextState(field)
    solve(updatedField, iterations, currIteration + 1, accumulatedFlashes + newFlashes)

@tailrec
def solve2(field: Field, currIteration: Int = 0): Int =
  val (updatedField, newFlashes) = nextState(field)
  if newFlashes == 100 then
    currIteration + 1
  else
    solve2(updatedField, currIteration + 1)

def nextState(field: Field): (Field, Int) =
  val increasedEnergy = increaseEnergy(field)
  val toFlash = increasedEnergy.filter(_._2 > 9).keys.toList
  val (updatedField, hasFlashed) = flash(toFlash, increasedEnergy, Set())
  val resetAllFlashed: Field = updatedField.filter((pos, energy) => hasFlashed.contains(pos)).map((pos, energy) => (pos, 0))
  (updatedField ++ resetAllFlashed, hasFlashed.size)

def increaseEnergy(field: Field): Field =
  field.map((pos, energy) => (pos, energy + 1))

@tailrec
def flash(toFlash: List[Position], field: Field, hasFlashed: Set[Position]): (Field, Set[Position]) =
  if toFlash.isEmpty then
    (field, hasFlashed)
  else if hasFlashed.contains(toFlash.head) then
    flash(toFlash.tail, field, hasFlashed)
  else
    val neighbours = getNeighbourPositions(toFlash.head).toSet
    val updatedNeighbours: Field = field.filter((pos, energy) => neighbours.contains(pos)).map((pos, energy) => (pos, energy + 1))
    val neighboursToFlash: List[Position] = updatedNeighbours.filter((pos, energy) => energy > 9 && !hasFlashed(pos)).keys.toList
    flash(neighboursToFlash ++ toFlash.tail, field ++ updatedNeighbours, hasFlashed + toFlash.head)

def getNeighbourPositions(p: Position): List[Position] =
  List(
    Position(p.x - 1, p.y - 1), Position(p.x - 1, p.y), Position(p.x - 1, p.y + 1),
    Position(p.x, p.y - 1), Position(p.x, p.y + 1),
    Position(p.x + 1, p.y - 1), Position(p.x + 1, p.y), Position(p.x + 1, p.y + 1),
  )

@main
def main(): Unit =
  val input = Source.fromResource("day11.txt").getLines().toList
  //val input = example.split("\n").toList
  val intLines = input.map(_.toCharArray.map(c => c.asDigit))
  val map = Map.from(intLines.zipWithIndex.flatMap(el => el._1.zipWithIndex.map(el2 => (Position(el._2, el2._2), el2._1.toInt))))

  println("Pt1: " + solve(map, 100))
  println("Pt2: " + solve2(map))

def printField(field: Field): Unit =
  (0 until 10).foreach(x => {
    (0 until 10).foreach(y => print(field.getOrElse(Position(x, y), -1)))
    println("")
  })

val example =
  """5483143223
    |2745854711
    |5264556173
    |6141336146
    |6357385478
    |4167524645
    |2176841721
    |6882881134
    |4846848554
    |5283751526
    |""".stripMargin
