package day6

import scala.annotation.tailrec
import scala.io.Source

type State = Map[Int, BigInt]

def default = BigInt(0)

@tailrec
def solve(state: State, iterations: Int): BigInt =
  if iterations == 0 then
    state.values.sum
  else
    solve(nextState(state), iterations - 1)

def nextState(state: State): State =
  Map(
    (8, state.getOrElse(0, default)),
    (7, state.getOrElse(8, default)),
    (6, state.getOrElse(7, default) + state.getOrElse(0, default)),
    (5, state.getOrElse(6, default)),
    (4, state.getOrElse(5, default)),
    (3, state.getOrElse(4, default)),
    (2, state.getOrElse(3, default)),
    (1, state.getOrElse(2, default)),
    (0, state.getOrElse(1, default)),
  )

@main
def main(): Unit =
  val numbers = Source.fromResource("day6.txt").getLines().flatMap(_.split(",")).map(_.toInt).toList
  val initialState: State = numbers.groupBy(identity).map { (key, list) => (key, BigInt(list.size)) }

  println("Pt1: " + solve(initialState, 80))
  println("Pt2: " + solve(initialState, 256))

def example() = "3,4,3,1,2"