package day19

import scala.annotation.tailrec
import scala.io.Source
import scala.collection.mutable

type Rotated = Rotation | None

case class Scanner(name: String, bacon: List[Position], rotation: Rotated = None())

case class Position(x: Int, y: Int, z: Int)

case class Rotation(rotation: Int, translation: Position)

case class None()

val rotations = List(
  (p: Position) => Position(p.x, p.y, p.z),
  (p: Position) => Position(p.x, -p.z, p.y),
  (p: Position) => Position(p.x, -p.y, -p.z),
  (p: Position) => Position(p.x, p.z, -p.y),

  (p: Position) => Position(-p.x, -p.y, p.z),
  (p: Position) => Position(-p.x, -p.z, -p.y),
  (p: Position) => Position(-p.x, p.y, -p.z),
  (p: Position) => Position(-p.x, p.z, p.y),

  (p: Position) => Position(p.y, -p.x, p.z),
  (p: Position) => Position(p.y, p.z, p.x),
  (p: Position) => Position(p.y, p.x, -p.z),
  (p: Position) => Position(p.y, -p.z, -p.x),

  (p: Position) => Position(-p.y, p.x, p.z),
  (p: Position) => Position(-p.y, -p.z, p.x),
  (p: Position) => Position(-p.y, -p.x, -p.z),
  (p: Position) => Position(-p.y, p.z, -p.x),

  (p: Position) => Position(p.z, p.y, -p.x),
  (p: Position) => Position(p.z, p.x, p.y),
  (p: Position) => Position(p.z, -p.y, p.x),
  (p: Position) => Position(p.z, -p.x, -p.y),

  (p: Position) => Position(-p.z, -p.y, -p.x),
  (p: Position) => Position(-p.z, -p.x, p.y),
  (p: Position) => Position(-p.z, p.y, p.x),
  (p: Position) => Position(-p.z, p.x, -p.y),
).zipWithIndex

@tailrec
def parse(lines: List[String], scanners: List[Scanner] = List()): List[Scanner] =
  val parsedLines = lines.takeWhile(!_.isBlank)
  val beacons = parsedLines.tail.map(getCoordinates)
  val rest = lines.dropWhile(!_.isBlank)
  val newScanner = Scanner(lines.head, beacons)
  if rest.isEmpty then
    newScanner :: scanners
  else
    parse(rest.tail, Scanner(lines.head, beacons) :: scanners)

def getCoordinates(str: String): Position = str match {
  case s"$x,$y,$z" => Position(x.toInt, y.toInt, z.toInt)
  case _ => throw RuntimeException(s"Could not parse $str")
}

def mDistance(p1: Position, p2: Position): Int =
  Math.abs(p1.x - p2.x) + Math.abs(p1.y - p2.y) + Math.abs(p1.z - p2.z)

def comparePositions(p1: Position, p2: Position, p3: Position, p4: Position): Boolean =
  (p1.x - p2.x) == (p3.x - p4.x) &&
    (p1.y - p2.y) == (p3.y - p4.y) &&
    (p1.z - p2.z) == (p3.z - p4.z)

def getAllPairs(list: List[Position]): List[(Position, Position)] =
  for i <- list; j <- list if i != j yield (i, j)


def findCommonOneRotation(beacons1: List[Position], beacons2: List[Position]): List[((Position, Position), (Position, Position))] =
  var matchingPositions: List[((Position, Position), (Position, Position))] = List()
  for i <- beacons1.indices do
    for j <- i + 1 until beacons1.size do
      for k <- beacons2.indices do
        for l <- k + 1 until beacons2.size do
          val matching = comparePositions(beacons1(i), beacons1(j), beacons2(k), beacons2(l))
          if matching then
            matchingPositions = ((beacons1(i), beacons1(j)), (beacons2(k), beacons2(l))) :: matchingPositions

  matchingPositions

def findCommonBeacons(scanner1: Scanner, scanner2: Scanner): (Int, List[Position], List[Position]) =
  val beacons1 = scanner1.bacon.sortBy(_.x).sortBy(_.y).sortBy(_.z)
  val allRotations2 = rotations.map((f, i) => (i, scanner2.bacon.map(f(_)).sortBy(_.x).sortBy(_.y).sortBy(_.z)))

  val possibleRotations: List[(Int, List[((Position, Position), (Position, Position))])] = allRotations2.map((i, rot) => (i, findCommonOneRotation(beacons1, rot))).filterNot(_._2.isEmpty)
  if possibleRotations.isEmpty then
    (-1, List(), List())
  else
    val (rotationInd, rotations) = possibleRotations.maxBy(_._2.size)
    val (posFrom, matchingPositions) = rotations.groupBy(_._1._1).maxBy(_._2.size)

    val fromPositions1 = posFrom :: matchingPositions.map(_._1._2).distinct
    val fromPositions2 = (matchingPositions.map(_._2._1) ++ matchingPositions.map(_._2._2)).distinct
    (rotationInd, fromPositions1, fromPositions2)

def rotateScanner(rotationInd: Int, scanner: Scanner, translation: Position): Scanner =
  val (rotationFun, _) = rotations(rotationInd)
  val beacons = scanner.bacon.map(rotationFun)
  val translatedBeacons = beacons.map(b => Position(b.x + translation.x, b.y + translation.y, b.z + translation.z))
  Scanner(scanner.name, translatedBeacons, Rotation(rotationInd, translation))

def getTranslation(pos1: Position, pos2: Position): Position =
  Position(pos1.x - pos2.x, pos1.y - pos2.y, pos1.z - pos2.z)

def getDistance(r1: Rotated, r2: Rotated): Int = (r1, r2) match {
  case (Rotation(_, pos1), Rotation(_, pos2)) => mDistance(pos1, pos2)
  case (_, Rotation(_, pos)) => mDistance(Position(0, 0, 0), pos)
  case (Rotation(_, pos), _) => mDistance(Position(0, 0, 0), pos)
  case (_,_) => throw RuntimeException("This should not happen..")
}

@main
def main(): Unit =
  val lines = Source.fromResource("day19.txt").getLines().toList
  val lineScanners = mutable.ArraySeq.from(parse(lines).reverse)

  var commonBeacons: List[(Int, List[Position], List[Position])] = List()

  val normalizedScanners = mutable.Set(lineScanners.head)
  val notNormalizedScanners = mutable.Set.from(lineScanners.tail)
  val visited: mutable.Set[String] = mutable.Set()

  while notNormalizedScanners.nonEmpty && visited.size != lineScanners.size do {
    val scanner1 = normalizedScanners.filter((sc: Scanner) => !visited.contains(sc.name)).head
    val scannersNowNormalized: mutable.Set[Scanner] = mutable.Set()
    for scanner2 <- notNormalizedScanners do {
      val (rotationInd, beacons1, beacons2) = findCommonBeacons(scanner1, scanner2)
      if rotationInd != -1 && beacons1.size > 10 then
        val translation = getTranslation(beacons1.sortBy(_.x).sortBy(_.y).minBy(_.z), beacons2.sortBy(_.x).sortBy(_.y).minBy(_.z))
        val rotatedScanner = rotateScanner(rotationInd, scanner2, translation)
        scannersNowNormalized.addOne(scanner2)
        normalizedScanners.addOne(rotatedScanner)
    }
    notNormalizedScanners --= scannersNowNormalized
    visited.addOne(scanner1.name)
  }


  val beacons = normalizedScanners.flatMap(_.bacon)
  println(s"Pt 1: ${beacons.size}")

  val distances = for s1 <- normalizedScanners; s2 <- normalizedScanners if s1 != s2 yield getDistance(s1.rotation, s2.rotation)
  println(s"Pt 2: ${distances.max}") // 11946



val sameOrientations =
  """--- scanner 0 ---
    |-1,-1,1
    |-2,-2,2
    |-3,-3,3
    |-2,-3,1
    |5,6,-4
    |8,0,7
    |
    |--- scanner 0 ---
    |1,-1,1
    |2,-2,2
    |3,-3,3
    |2,-1,3
    |-5,4,-6
    |-8,-7,0
    |
    |--- scanner 0 ---
    |-1,-1,-1
    |-2,-2,-2
    |-3,-3,-3
    |-1,-3,-2
    |4,6,5
    |-7,0,8
    |
    |--- scanner 0 ---
    |1,1,-1
    |2,2,-2
    |3,3,-3
    |1,3,-2
    |-4,-6,5
    |7,0,8
    |
    |--- scanner 0 ---
    |1,1,1
    |2,2,2
    |3,3,3
    |3,1,2
    |-6,-4,-5
    |0,7,-8""".stripMargin.split("\n").toList
