package day9

import scala.annotation.tailrec
import scala.io.Source

case class Position(x: Int, y: Int)
type Height = Int
type Positions = Map[Position, Height]

def solve1(positions: Positions): Int =
  val lowestPositions = getLowestPositions(positions)
  lowestPositions.values.sum + lowestPositions.size

def solve2(positions: Positions): Int =
  val lowestPositions = getLowestPositions(positions)
  val basins = lowestPositions.map(pos => bfs(List(pos._1), positions, Set()))
  val basinsWithSizes = basins.map(basin => basin.size).toList.sorted.reverse
  basinsWithSizes.take(3).product

@tailrec
def bfs(toVisit: List[Position], map: Positions, visited: Set[Position]): Set[Position] =
  if toVisit.isEmpty then
    visited
  else
    val neighbours = getNeighbourPositions(toVisit.head)
    val nextToVisit = neighbours.filter(p => map.getOrElse(p, Int.MaxValue) < 9 && !visited(p))
    bfs(toVisit.tail ++ nextToVisit, map, visited + toVisit.head)


def getLowestPositions(positions: Positions): Positions =
  positions.filter((position, height) => isLowestPoint(position, height, positions))

def isLowestPoint(position: Position, height: Height, map: Positions): Boolean =
  val neighbourHeights = getNeighbourHeights(map, getNeighbourPositions(position))
  neighbourHeights.forall(h => h > height)

def getNeighbourHeights(positions: Positions, neighbourPositions: List[Position]): List[Height] =
  neighbourPositions.map(positions.getOrElse(_, Int.MaxValue))

def getNeighbourPositions(p: Position): List[Position] =
  List(
    Position(p.x - 1, p.y),
    Position(p.x + 1, p.y),
    Position(p.x, p.y - 1),
    Position(p.x, p.y + 1),
  )
def inputToMap(input: List[List[Char]]): Positions =
  input.zipWithIndex.flatMap(row => row._1.zipWithIndex.map(el => (Position(row._2, el._2), el._1.toInt-48))).toMap


@main
def main(): Unit =
  val input = Source.fromResource("day9.txt").getLines().map(s => s.toCharArray.toList).toList
  //val input = example.split("\n").map(s => s.toCharArray.toList).toList
  val map = inputToMap(input)

  println("Pt1: " + solve1(map))
  println("Pt2: " + solve2(map))

val example =
  """
    |2199943210
    |3987894921
    |9856789892
    |8767896789
    |9899965678
    |""".stripMargin
