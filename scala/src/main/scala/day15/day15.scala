package day15

import scala.annotation.tailrec
import scala.io.Source

case class Position(x: Int, y: Int)
case class Cave(graph: Graph, start: Position, end: Position)
type Graph = Map[Position, Int]

def solve(graph: Graph): Int =
  val start = graph(Position(0, 0))
  val end = getEnd(graph)
  dijkstra(Cave(graph, Position(0, 0), end), graph.keys.toSet, Map((Position(0, 0), 0)))

@tailrec
def dijkstra(cave: Cave, toVisit: Set[Position], distances: Graph, visited: Set[Position] = Set()): Int =
  val nextToVisit = getLowestDistanceCandidate(distances, toVisit)
  if nextToVisit == cave.end then
    distances(nextToVisit)
  else
    val neighbours = getNeighbourPositions(cave, nextToVisit).filterNot(visited.contains)
    val neighboursUpdatedDistance = neighbours.map(updateDistance(cave.graph, distances, nextToVisit))
    val updatedDistances = distances ++ neighboursUpdatedDistance
    dijkstra(cave, toVisit - nextToVisit, updatedDistances, visited + nextToVisit)

def getEnd(graph: Graph): Position =
  graph.keys.groupBy(_.x).maxBy(_._1)._2.maxBy(_.y)

def updateDistance(graph: Graph, distances: Graph, currentPosition: Position)(position: Position): (Position, Int) =
  val distance = distances.getOrElse(position, Int.MaxValue)
  val newDistance = List(distance, graph(position) + distances(currentPosition)).min
  (position, newDistance)

def getLowestDistanceCandidate(graph: Graph, toVisit: Set[Position]): Position =
  toVisit.map(p => (p, graph.getOrElse(p, Int.MaxValue))).minBy(_._2)._1

def getNeighbourPositions(cave: Cave, p: Position): List[Position] =
  List(
    Position(p.x - 1, p.y),
    Position(p.x, p.y - 1), Position(p.x, p.y + 1),
    Position(p.x + 1, p.y),
  ).filter(p => p.x >= cave.start.x && p.y >= cave.start.y && p.x <= cave.end.x && p.y <= cave.end.y)


def parseInput(lines: List[String]): Graph =
  lines
    .zipWithIndex
    .flatMap((line:String, y:Int) =>
      line.toCharArray
        .map(_.asDigit)
        .zipWithIndex
        .map((cost:Int, x:Int) => (Position(x, y), cost)))
    .toMap

@main
def main(): Unit =
  val lines = Source.fromResource("day15.txt").getLines().filterNot(_.isBlank).toList
  //val lines = example.split("\n").filterNot(_.isBlank).toList
  val graph = parseInput(lines)

  println("Pt1: " + solve(graph))

val example =
  """
    |1163751742
    |1381373672
    |2136511328
    |3694931569
    |7463417111
    |1319128137
    |1359912421
    |3125421639
    |1293138521
    |2311944581
    |""".stripMargin