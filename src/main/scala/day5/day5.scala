package day5

import scala.io.Source

case class Line(start: Coord, stop: Coord):
  val maxX: Int = Math.max(start.x, stop.x)
  val maxY: Int = Math.max(start.y, stop.y)
  val minX: Int = Math.min(start.x, stop.x)
  val minY: Int = Math.min(start.y, stop.y)
  val isHorizontal: Boolean = start.y == stop.y
  val isVertical: Boolean = start.x == stop.x

case class Coord(x: Int, y: Int)


def countIntersections(lines: List[Line]): Int =
  lines.flatMap(generateAllPoints).groupBy(identity).values.count(_.size > 1)

def generateAllPoints(line: Line): List[Coord] =
  if line.isHorizontal then
    List.range(line.minX, line.maxX + 1).map(Coord(_, line.minY))
  else if line.isVertical then
    List.range(line.minY, line.maxY + 1).map(Coord(line.minX, _))
  else
    val xInc = if line.start.x > line.stop.x then -1 else 1
    val yInc = if line.start.y > line.stop.y then -1 else 1
    val xRange = (line.start.x to line.stop.x by xInc).toList
    val yRange = (line.start.y to line.stop.y by yInc).toList
    xRange zip yRange map { (x, y) => Coord(x, y)}

def parseInput(line: String): Line = line match {
  case s"$x1,$y1 -> $x2,$y2" => Line(Coord(x1.toInt, y1.toInt), Coord(x2.toInt, y2.toInt))
  case _ => throw RuntimeException(s"Not matching: $line")
}

@main
def main(): Unit =
  val input = Source.fromResource("day5.txt").getLines().toList
  val lines = input.map(parseInput)

  val filteredLines = lines.filter(l => l.isVertical || l.isHorizontal)

  println("Pt1: " + countIntersections(filteredLines))
  println("Pt2: " + countIntersections(lines))

def example(): List[String] =
  """
    |0,9 -> 5,9
    |8,0 -> 0,8
    |9,4 -> 3,4
    |2,2 -> 2,1
    |7,0 -> 7,4
    |6,4 -> 2,0
    |0,9 -> 2,9
    |3,4 -> 1,4
    |0,0 -> 8,8
    |5,5 -> 8,2
    |""".stripMargin.stripLeading.stripTrailing.split("\n").toList
