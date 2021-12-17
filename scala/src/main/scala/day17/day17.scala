package day17

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

def matchInput(str: String): ((Int, Int), (Int, Int)) = str match {
  case s"target area: x=$xMin..$xMax, y=$yMin..$yMax" => ((xMin.toInt, xMax.toInt), (yMin.toInt, yMax.toInt))
  case _ => throw RuntimeException(s"Could not parse '$str'")
}

def getY(currY: Int, initialVel: Int, t: Int): Int = currY + (initialVel - t + 1)
def getX(currX: Int, initialVel: Int, t: Int): Int = currX + Math.max(0, initialVel - t + 1)

def stopY(yMin: Int)(currY: Int, t: Int): Boolean = currY < yMin
def stopX(xMax: Int)(currX: Int, t: Int): Boolean = currX > xMax || t > 300

@tailrec
def calculateVelocities(startVelocity: Int,
                        stopFun: (Int, Int) => Boolean,
                        newVelocityFun: (Int, Int, Int) => Int,
                        currPosition: Int = 0,
                        t: Int = 1,
                        acc: List[(Int, Int, Int)] = List()
                       ): List[(Int, Int, Int)] =
  if stopFun(currPosition, t) then
    acc.reverse
  else
    calculateVelocities(startVelocity, stopFun, newVelocityFun, newVelocityFun(currPosition, startVelocity, t), t + 1, (startVelocity, currPosition, t) :: acc)

def combine(xVelocities: Map[Int, List[Int]], yVelocities: Map[Int, List[Int]], t: Int): List[(Int, Int)] =
  for x <- xVelocities(t); y <- yVelocities(t) yield (x, y)

def getVelocitiesHittingBox(min: Int, max: Int, velocities: List[(Int, Int, Int)]): List[(Int, Int, Int)] =
  velocities.filter((sv, v, t) => v <= max && v >= min)

def getVelocitiesGroupedByTime(velocities: List[(Int, Int, Int)]): Map[Int, List[Int]] =
  velocities.groupBy(_._3).map((k, v) => (k, v.map((a, v, c) => a)))

@main
def main(): Unit =
  val line = "target area: x=155..215, y=-132..-72"
  //val line = "target area: x=20..30, y=-10..-5"
  val (x, y) = matchInput(line)
  val xMax = Math.max(x._1, x._2)
  val xMin = Math.min(x._1, x._2)
  val yMax = Math.max(y._1, y._2)
  val yMin = Math.min(y._1, y._2)

  val xVelocities = (0 to xMax).flatMap(calculateVelocities(_, stopX(xMax), getX)).toList
  val yVelocitiesNotFlat = (-500 to 1000).map(calculateVelocities(_, stopY(yMin), getY)).toList
  val yVelocities = yVelocitiesNotFlat.flatten

  val xVelocitiesHittingBox = getVelocitiesHittingBox(xMin, xMax, xVelocities)
  val yVelocitiesHittingBox = getVelocitiesHittingBox(yMin, yMax, yVelocities)

  val xVelMappedByTime = getVelocitiesGroupedByTime(xVelocitiesHittingBox)
  val yVelMappedByTime = getVelocitiesGroupedByTime(yVelocitiesHittingBox)

  val sharedTimes = xVelMappedByTime.keySet.intersect(yVelMappedByTime.keySet)
  val combinedVelocities = sharedTimes.flatMap(combine(xVelMappedByTime, yVelMappedByTime, _))

  val maxY = yVelocitiesNotFlat
    .filterNot(list => getVelocitiesHittingBox(yMin, yMax, list).isEmpty)
    .map(list => list.maxBy(_._2))
    .map(_._2)
    .max

  println(s"Pt 1: $maxY")
  println(s"Pt 2: ${combinedVelocities.size}")




