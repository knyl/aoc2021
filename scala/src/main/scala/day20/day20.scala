package day20

import scala.io.Source

type TrenchMap = Map[Position, Char]

case class Position(x: Int, y: Int)

def solve1(imageEnchancement: String, map: TrenchMap, defaultFun: Int => Char, iterations: Int): Int =
  val newMap = (1 to iterations).foldLeft(map)((aMap, i) => enchanceMap(imageEnchancement, aMap, defaultFun(i)))
  newMap.values.count(_ == '#')

def enchanceMap(imageEnchancement: String, map: TrenchMap, default: Char): TrenchMap =
  val borderPositions = getBorderPositions(map)
  val allPositions = borderPositions ++ map.keys
  allPositions.map(enchanceMap(imageEnchancement, map, _, default)).toMap

def enchanceMap(imageEnchancement: String, map: TrenchMap, pos: Position, default: Char): (Position, Char) =
  val binaryString =
    getWithNeighbours(pos)
      .map(map.getOrElse(_, default))
      .mkString
      .replace('.', '0')
      .replace('#', '1')
  val enchancementIndex = Integer.parseInt(binaryString, 2)
  (pos, imageEnchancement.charAt(enchancementIndex))

def getWithNeighbours(p: Position): List[Position] =
  List(
    Position(p.x - 1, p.y - 1), Position(p.x - 1, p.y), Position(p.x - 1, p.y + 1),
    Position(p.x, p.y - 1), Position(p.x, p.y), Position(p.x, p.y + 1),
    Position(p.x + 1, p.y - 1), Position(p.x + 1, p.y), Position(p.x + 1, p.y + 1),
  )

def getBorderPositions(map: TrenchMap): Iterable[Position] =
  val maxX = map.keys.maxBy(_._1).x
  val maxY = map.keys.maxBy(_._2).y
  val minX = map.keys.minBy(_._1).x
  val minY = map.keys.minBy(_._2).y
  val topPositions = ((minX - 1) to maxX + 1).map(Position(_, minY - 1))
  val bottomPositions = ((minX - 1) to maxX + 1).map(Position(_, maxY + 1))
  val leftSide = (minY to maxY).map(Position(minX - 1, _))
  val rightSide = (minY to maxY).map(Position(maxX + 1, _))
  topPositions ++ bottomPositions ++ leftSide ++ rightSide

def toMap(strings: Iterable[String]): TrenchMap =
  strings
    .zipWithIndex
    .flatMap((str: String, x: Int) => str.zipWithIndex.map((c: Char, y: Int) => (Position(x, y), c)))
    .toMap

@main
def main(): Unit =
  val lines = Source.fromResource("day20.txt").getLines().toList
  val imageEnchancement = lines.head
  val map = toMap(lines.tail.tail)

  val exEnchancement = example.head
  val exMap = toMap(example.tail.tail)


  println(s"Pt 1 ex: ${solve1(exEnchancement, exMap, i => '.', 2)}")
  println(s"Pt 2 ex: ${solve1(exEnchancement, exMap, i => '.', 50)}")

  println()

  val defaultFun: Int => Char = i => if i % 2 == 1 then '.' else '#'
  println(s"Pt 1: ${solve1(imageEnchancement, map, defaultFun, 2)}")
  println(s"Pt 2: ${solve1(imageEnchancement, map, defaultFun, 50)}")


val example =
  """
    |..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#
    |
    |#..#.
    |#....
    |##..#
    |..#..
    |..###
    |""".stripMargin.strip.split("\n")