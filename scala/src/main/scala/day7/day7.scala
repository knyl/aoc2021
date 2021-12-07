package day7

import scala.io.Source

def solve(numbers: List[Int], fuelCostFun: (Int, Int) => Int): Int =
  val offset = numbers.head
  val max = numbers.last
  val fuels = numbers.foldLeft(List.fill(max - offset + 1)(0))((acc, crabPosition) => acc.zipWithIndex.map((fuel, index) => fuel + fuelCostFun(crabPosition, index + offset)))
  fuels.min

def getFuelCost(crabPosition: Int, position: Int): Int =
  Math.abs(position - crabPosition)

def getFuelCost2(crabPosition: Int, position: Int): Int =
  val stepsToTake = Math.abs(position - crabPosition)
  (1 to stepsToTake).sum

@main
def main(): Unit =
  val numbers = Source.fromResource("day7.txt").getLines().flatMap(_.split(",")).map(_.toInt).toList.sorted

  println("Pt1: " + solve(numbers, getFuelCost))
  println("Pt2: " + solve(numbers, getFuelCost2))
