package day22

import scala.annotation.tailrec
import scala.io.Source

case class Instruction(operation: Operation, cube: Cube)

case class Position(x: Int, y: Int, z: Int)

case class Cube(x1: Int, x2: Int, y1: Int, y2: Int, z1: Int, z2: Int, exclusions: List[Cube] = List())

enum Operation:
  case ON, OFF

def inRange(i: Int): Boolean = i >= -50 && i <= 50

def generatePositions(cube: Cube): Set[Position] =
  if inRange(cube.x1) && inRange(cube.x2) && inRange(cube.y1) && inRange(cube.y2) && inRange(cube.z1) && inRange(cube.z2) then
    (for x <- cube.x1 to cube.x2; y <- cube.y1 to cube.y2; z <- cube.z1 to cube.z2 yield Position(x, y, z)).toSet
  else
    Set()

def parseInput(string: String): Instruction = string match {
  case s"on x=$xMin..$xMax,y=$yMin..$yMax,z=$zMin..$zMax" => Instruction(Operation.ON, Cube(xMin.toInt, xMax.toInt, yMin.toInt, yMax.toInt, zMin.toInt, zMax.toInt))
  case s"off x=$xMin..$xMax,y=$yMin..$yMax,z=$zMin..$zMax" => Instruction(Operation.OFF, Cube(xMin.toInt, xMax.toInt, yMin.toInt, yMax.toInt, zMin.toInt, zMax.toInt))
}

def doInstruction(cubes: Set[Position], instruction: Instruction): Set[Position] = instruction match {
  case Instruction(Operation.ON, range) => cubes.union(generatePositions(range))
  case Instruction(Operation.OFF, range) => cubes.diff(generatePositions(range))
}

def overlap(p1: Int, p2: Int, p3: Int, p4: Int): Boolean =
  (p1 to p2).contains(p3) || (p1 to p2).contains(p4) ||
    (p3 to p4).contains(p1) || (p3 to p4).contains(p2)

def splitCube(cube1: Cube, cube2: Cube): List[Cube] = intersects(cube1, cube2) match {
  case None => List(cube1)
  case option => splitIntoParts(cube1, option.get)
}

def splitIntoParts(cube: Cube, intersection: Cube): List[Cube] =
  var cubes: List[Cube] = List()
  if intersection.x1 > cube.x1 then
    cubes = cube.copy(x2 = intersection.x1 - 1) :: cubes
  if intersection.x2 < cube.x2 then
    cubes = cube.copy(x1 = intersection.x2 + 1) :: cubes
  if intersection.y1 > cube.y1 then
    cubes = cube.copy(x1 = intersection.x1, x2 = intersection.x2, y2 = intersection.y1 - 1) :: cubes
  if intersection.y2 < cube.y2 then
    cubes = cube.copy(x1 = intersection.x1, x2 = intersection.x2, y1 = intersection.y2 + 1) :: cubes
  if intersection.z1 > cube.z1 then
    cubes = intersection.copy(z2 = intersection.z1 - 1, z1 = cube.z1) :: cubes
  if intersection.z2 < cube.z2 then
    cubes = intersection.copy(z1 = intersection.z2 + 1, z2 = cube.z2) :: cubes
  cubes

def getOverlap(p1: Int, p2: Int, p3: Int, p4: Int): (Int, Int) =
  val p1Min = math.min(p1, p2)
  val p1Max = math.max(p1, p2)
  val p3Min = math.min(p3, p4)
  val p3Max = math.max(p3, p4)
  (math.max(p1Min, p3Min), math.min(p1Max, p3Max))

def getCubeOverlap(cube1: Cube, cube2: Cube): Cube =
  val (xMin, xMax) = getOverlap(cube1.x1, cube1.x2, cube2.x1, cube2.x2)
  val (yMin, yMax) = getOverlap(cube1.y1, cube1.y2, cube2.y1, cube2.y2)
  val (zMin, zMax) = getOverlap(cube1.z1, cube1.z2, cube2.z1, cube2.z2)
  Cube(xMin, xMax, yMin, yMax, zMin, zMax)

def intersects(cube1: Cube, cube2: Cube): Option[Cube] =
  if overlap(cube1.x1, cube1.x2, cube2.x1, cube2.x2) &&
    overlap(cube1.y1, cube1.y2, cube2.y1, cube2.y2) &&
    overlap(cube1.z1, cube1.z2, cube2.z1, cube2.z2) then
    val foo = getCubeOverlap(cube1, cube2)
    Option(foo)
  else
    None

def applyInstruction(cubes: List[Cube], instruction: Instruction): List[Cube] = instruction match {
  case Instruction(Operation.ON, cube) => cube :: cubes.flatMap(splitCube(_, cube))
  case Instruction(Operation.OFF, cube) => cubes.flatMap(splitCube(_, cube))
}

def getVolume(cube: Cube): Long =
  (math.abs(cube.x2 - cube.x1) + 1).toLong * (math.abs(cube.y2 - cube.y1) + 1).toLong * (math.abs(cube.z2 - cube.z1) + 1).toLong

def solve2(instructions: Iterable[Instruction]): Long =
  val cubes = instructions.tail.foldLeft(List(instructions.head.cube))(applyInstruction)
  val volume = cubes.map(getVolume).sum
  volume

@main
def main(): Unit =
  val lines = Source.fromResource("day22.txt").getLines().toList
  val instructions = lines.map(parseInput)

  val result = instructions.foldLeft(Set(): Set[Position])(doInstruction)
  println(s"Pt 1: ${result.size}")

  val result2 = solve2(instructions)
  println(s"Pt 2: $result2")
