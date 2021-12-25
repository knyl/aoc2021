package day25

import scala.annotation.tailrec
import scala.io.Source

type Board = Map[Position, Cucumber]

case class State(board: Board, xMax: Int, yMax: Int)

case class Position(x: Int, y: Int)

enum Cucumber:
  case EAST, SOUTH

def parseInput(lines: Iterable[String]): State =
  val board =
    lines
      .zipWithIndex
      .flatMap((l: String, y: Int) => l.zipWithIndex.flatMap((c: Char, x: Int) => getCucumber(x, y, c)))
      .toMap
  State(board, lines.head.length, lines.size)


def getCucumber(x: Int, y: Int, c: Char): Option[(Position, Cucumber)] = c match {
  case 'v' => Option((Position(x, y), Cucumber.SOUTH))
  case '>' => Option((Position(x, y), Cucumber.EAST))
  case _ => Option.empty
}

@tailrec
def solve(state: State, currIteration: Int = 1): Int =
  val newState = move(state)
  if state.board == newState.board then
    currIteration + 1
  else
    solve(newState, currIteration + 1)

def move(state: State): State =
  val state0 = moveDirection(state, Cucumber.EAST)
  moveDirection(state0, Cucumber.SOUTH)

def moveDirection(state: State, cucumber: Cucumber): State =
  val newBoard =
    state.board.map((p, c) => {
      val newPos = getNeighbour(p, cucumber, state)
      if c == cucumber && !state.board.contains(newPos) then (newPos, c)
      else (p, c)
    })
  state.copy(board = newBoard)

def getNeighbour(position: Position, cucumber: Cucumber, state: State): Position = cucumber match {
  case Cucumber.EAST => position.copy(x = (position.x + 1) % state.xMax)
  case Cucumber.SOUTH => position.copy(y = (position.y + 1) % state.yMax)
}


@main
def main(): Unit =
  val lines = Source.fromResource("day25.txt").getLines().filterNot(_.isBlank).toList
  val board = parseInput(lines)
  val result = solve(board, 0)
  println(result)


def printState(state: State): Unit =
  (0 until state.yMax).foreach(y =>
    println((0 until state.xMax).map(x =>
      getChar(state, x, y)
    ).toList.mkString)
  )

def getChar(state: State, x: Int, y: Int): Char =
  if !state.board.contains(Position(x, y)) then '.'
  else if state.board(Position(x, y)) == Cucumber.EAST then '>'
  else 'v'