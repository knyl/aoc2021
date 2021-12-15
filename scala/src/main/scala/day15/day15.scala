package day15

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

case class Position(x: Int, y: Int)

case class Cave(graph: Graph, start: Position, end: Position)

type Graph = Map[Position, Int]

def solve(graph: Graph): Int =
  val start = graph(Position(0, 0))
  val end = getEnd(graph)
  val toVisit = mutable.PriorityQueue.empty[(Position, Int)](
    Ordering.by((_: (Position, Int))._2).reverse
  )
  toVisit.enqueue((Position(0, 0), 0))
  dijkstra(Cave(graph, Position(0, 0), end), toVisit, Map((Position(0, 0), 0)))

@tailrec
def dijkstra(cave: Cave, toVisit: mutable.PriorityQueue[(Position, Int)], distances: Graph, visited: Set[Position] = Set()): Int =
  val (nextToVisit, _) = getNext(toVisit, visited)
  if nextToVisit == cave.end then
    distances(nextToVisit)
  else
    val neighbours = getNeighbourPositions(cave, nextToVisit).filterNot(visited.contains)
    val neighboursUpdatedDistance = neighbours.map(updateDistance(cave.graph, distances, nextToVisit))
    val updatedDistances = distances ++ neighboursUpdatedDistance
    neighboursUpdatedDistance.foreach(toVisit.enqueue(_))
    dijkstra(cave, toVisit, updatedDistances, visited + nextToVisit)

def getNext(toVisit: mutable.PriorityQueue[(Position, Int)], visited: Set[Position]): (Position, Int) =
  var next = toVisit.dequeue()
  while (visited.contains(next._1))
    next = toVisit.dequeue()
  next

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

def makeBigGraph(graph: Graph): Graph =
  (0 until 5).flatMap(x => (0 until 5).flatMap(y => getNewNodeList(graph, x, y))).toMap

def getNewNodeList(graph: Graph, x: Int, y: Int): List[(Position, Int)] =
  graph.toList.map((p: Position, d: Int) => (Position(p.x + x * 100, p.y + y * 100), getDist(x, y, d)))

def getDist(x: Int, y: Int, d: Int): Int =
  val dist = x + y + d
  if dist > 9 then dist % 10 + 1 else dist

def parseInput(lines: List[String]): Graph =
  lines
    .zipWithIndex
    .flatMap((line: String, y: Int) =>
      line.toCharArray
        .map(_.asDigit)
        .zipWithIndex
        .map((cost: Int, x: Int) => (Position(x, y), cost)))
    .toMap

@main
def main(): Unit =
  val lines = Source.fromResource("day15.txt").getLines().filterNot(_.isBlank).toList
  //val lines = example.split("\n").filterNot(_.isBlank).toList
  val graph = parseInput(lines)

  println("Pt1: " + solve(graph))
  println("Pt2: " + solve(makeBigGraph(graph)))

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