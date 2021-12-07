package day7

import scala.io.Source

def solve(numbers: List[Int], fuelCostFun: (Int, Int, Int) => Int): Int =
  val min = numbers.head
  val max = numbers.last
  val fuels = numbers.foldLeft(List.fill(max - min + 1)(0))((acc, number) => acc.zipWithIndex.map((fuel, int) => fuel + fuelCostFun(number, int, min)))
  fuels.min

def getFuelCost(crabPosition: Int, currentIndex: Int, listMin: Int): Int =
  Math.abs(listMin + currentIndex - crabPosition)

def getFuelCost2(crabPosition: Int, currentIndex: Int, listMin: Int): Int =
  val stepsToTake = Math.abs(listMin + currentIndex - crabPosition)
  (1 to stepsToTake).sum

@main
def main(): Unit =
  val numbers = Source.fromResource("day7.txt").getLines().flatMap(_.split(",")).map(_.toInt).toList.sorted
  val example = "16,1,2,0,4,2,7,1,2,14"

  println("Pt1: " + solve(numbers, getFuelCost))
  println("Pt2: " + solve(numbers, getFuelCost2))
