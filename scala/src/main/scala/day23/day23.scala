package day23

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.collection.mutable.PriorityQueue

case class State(board: Board, score: Int)

case class Board(pieces: Set[Piece])

case class Position(x: Int, y: Int)

case class Piece(podType: Type, pos: Position, done: Boolean = false)

enum Type:
  case A, B, C, D

def isDone(board: Board): Boolean =
  val finalPositions = Set(
    Piece(Type.A, Position(2, 1), true),
    Piece(Type.A, Position(2, 2), true),
    Piece(Type.A, Position(2, 3), true),
    Piece(Type.A, Position(2, 4), true),

    Piece(Type.B, Position(4, 1), true),
    Piece(Type.B, Position(4, 2), true),
    Piece(Type.B, Position(4, 3), true),
    Piece(Type.B, Position(4, 4), true),

    Piece(Type.C, Position(6, 1), true),
    Piece(Type.C, Position(6, 2), true),
    Piece(Type.C, Position(6, 3), true),
    Piece(Type.C, Position(6, 4), true),

    Piece(Type.D, Position(8, 1), true),
    Piece(Type.D, Position(8, 2), true),
    Piece(Type.D, Position(8, 3), true),
    Piece(Type.D, Position(8, 4), true),
  )
  finalPositions.subsetOf(board.pieces)

def getEndRoom(piece: Piece): Int = piece.podType match {
  case Type.A => 2
  case Type.B => 4
  case Type.C => 6
  case Type.D => 8
}

def isInRoom(piece: Piece): Boolean = piece.pos.x match {
  case 2 | 4 | 6 | 8 => true
  case _ => false
}

def hasPieceAbove(piece: Piece, state: State): Boolean =
  val yAbove = (piece.pos.y - 1) to 1 by -1
  state.board.pieces.exists(p => p.pos.x == piece.pos.x && yAbove.contains(p.pos.y))

def isMovableInRoom(piece: Piece, state: State): Boolean =
  piece.pos.y == 1 || !hasPieceAbove(piece, state)

def movements(state: State): Set[State] =
  state.board.pieces.flatMap(movementsAmphipod(_, state))

def isInRoomAndMovable(piece: Piece, state: State): Boolean =
  isInRoom(piece) && isMovableInRoom(piece, state)

def isAbove(piece: Piece): Boolean = piece.pos.y == 0

def roomIsReady(piece: Piece, state: State): Boolean =
  val roomCoord = getEndRoom(piece)
  val piecesInRoom = state.board.pieces.filter(p => p.pos.x == roomCoord)
  piecesInRoom.forall(_.podType == piece.podType)

def getPositionsBetween(piecePos: Int, roomPos: Int): Set[Int] =
  val abovePositions = Set(0, 1, 3, 5, 7, 9, 10)
  if piecePos < roomPos then abovePositions.filter(p => p > piecePos && p < roomPos)
  else abovePositions.filter(p => p > roomPos && p < piecePos)

def wayToRoomIsClear(piece: Piece, state: State): Boolean =
  val xDiff = getPositionsBetween(piece.pos.x, getEndRoom(piece))
  !state.board.pieces.exists(p => xDiff.contains(p.pos.x))

def isMovableAbove(piece: Piece, state: State): Boolean =
  roomIsReady(piece, state) && wayToRoomIsClear(piece, state)

def isAboveAndMovable(piece: Piece, state: State): Boolean =
  isAbove(piece) && isMovableAbove(piece, state)

def getFreePositionsLeft(piece: Piece, state: State): List[Int] =
  val topPieces = state.board.pieces.filter(isAbove).filter(_.pos.x < piece.pos.x).toList.sortBy(_.pos.x)
  val abovePositions = List(0, 1, 3, 5, 7, 9, 10).filter(_ < piece.pos.x)
  if topPieces.isEmpty then abovePositions
  else
    val maxPos = topPieces.maxBy(_.pos.x)
    getPositionsBetween(maxPos.pos.x, piece.pos.x).toList


def getFreePositionsRight(piece: Piece, state: State): List[Int] =
  val topPieces = state.board.pieces.filter(isAbove).filter(_.pos.x > piece.pos.x).toList.sortBy(_.pos.x)
  val abovePositions = List(0, 1, 3, 5, 7, 9, 10).filter(_ > piece.pos.x)
  if topPieces.isEmpty then abovePositions
  else
    val minPos = topPieces.minBy(_.pos.x)
    getPositionsBetween(minPos.pos.x, piece.pos.x).toList

def newStateForPosition(position: Position, piece: Piece, state: State, done: Boolean = false): State =
  val steps = stepsToPosition(piece, position)
  val score = getScore(steps, piece)
  val newBoard = Board(state.board.pieces - piece + Piece(piece.podType, position, done))
  State(newBoard, state.score + score)


def generateMovementsFromRoom(piece: Piece, state: State): List[State] =
  val abovePositions = Set(0, 1, 3, 5, 7, 9, 10)
  val freePositionsLeft = getFreePositionsLeft(piece, state)
  val freePositionsRight = getFreePositionsRight(piece, state)
  val newPositions = (freePositionsLeft ++ freePositionsRight).map(Position(_, 0))
  newPositions.map(newStateForPosition(_, piece, state))

def getFirstFreePositionInRoom(piece: Piece, state: State): Position =
  val roomCoord = getEndRoom(piece)
  val piecesInRoom = state.board.pieces.filter(p => p.pos.x == roomCoord)
  if piecesInRoom.isEmpty then
    Position(roomCoord, 4)
  else
    val topPiece = piecesInRoom.minBy(_.pos.y)
    Position(topPiece.pos.x, topPiece.pos.y - 1)

def stepsToPosition(piece: Piece, position: Position): Int =
  math.abs(piece.pos.x - position.x) + math.abs(piece.pos.y - position.y)

def getScore(steps: Int, piece: Piece): Int = piece.podType match {
  case Type.A => steps
  case Type.B => steps * 10
  case Type.C => steps * 100
  case Type.D => steps * 1000
}

def generateMovementsFromAbove(piece: Piece, state: State): List[State] =
  val firstFreePositionInRoom = getFirstFreePositionInRoom(piece, state)
  val newState = newStateForPosition(firstFreePositionInRoom, piece, state, true)
  List(newState)

def movementsAmphipod(piece: Piece, state: State): List[State] =
  if piece.done then List()
  else if isInRoomAndMovable(piece, state) then generateMovementsFromRoom(piece, state)
  else if isAboveAndMovable(piece, state) then generateMovementsFromAbove(piece, state)
  else List()

@tailrec
def solve(states: mutable.PriorityQueue[State], visited: Set[Board] = Set(), it: Int = 0): Int =
  val nextState = states.dequeue()
  if isDone(nextState.board) then
    nextState.score
  else
    if !visited.contains(nextState.board) then
      val newVisited = visited + nextState.board
      val newStates = movements(nextState)
      states.addAll(newStates)
      solve(states, newVisited, it + 1)
    else
      solve(states, visited, it + 1)

@main
def main(): Unit =
  val toVisit = mutable.PriorityQueue.empty[State](
    Ordering.by((_: State).score).reverse
  )
  toVisit.addOne(State(board = initialState, score = 0))
  println("Pt 1: 14627 (with pen and paper..)")
  println(s"Pt 2: ${solve(toVisit)}")


val initialState = Board(
  Set(
    Piece(Type.D, Position(2, 1)),
    Piece(Type.D, Position(2, 2)),
    Piece(Type.D, Position(2, 3)),
    Piece(Type.C, Position(2, 4)),

    Piece(Type.B, Position(4, 1)),
    Piece(Type.C, Position(4, 2)),
    Piece(Type.B, Position(4, 3)),
    Piece(Type.A, Position(4, 4)),

    Piece(Type.D, Position(6, 1)),
    Piece(Type.B, Position(6, 2)),
    Piece(Type.A, Position(6, 3)),
    Piece(Type.A, Position(6, 4)),

    Piece(Type.B, Position(8, 1)),
    Piece(Type.A, Position(8, 2)),
    Piece(Type.C, Position(8, 3)),
    Piece(Type.C, Position(8, 4)),
  )
)

val input =
  """
    |#############
    |#...........#
    |###D#B#D#B###
    |  #D#C#B#A#
    |  #D#B#A#C#
    |  #C#A#A#C#
    |  #########
    |""".stripMargin.split("\n").filterNot(_.isBlank)

val initialStateEx = Board(
  Set(
    Piece(Type.B, Position(2, 1)),
    Piece(Type.D, Position(2, 2)),
    Piece(Type.D, Position(2, 3)),
    Piece(Type.A, Position(2, 4), true),

    Piece(Type.C, Position(4, 1)),
    Piece(Type.C, Position(4, 2)),
    Piece(Type.B, Position(4, 3)),
    Piece(Type.D, Position(4, 4)),

    Piece(Type.B, Position(6, 1)),
    Piece(Type.B, Position(6, 2)),
    Piece(Type.A, Position(6, 3)),
    Piece(Type.C, Position(6, 4), true),

    Piece(Type.D, Position(8, 1)),
    Piece(Type.A, Position(8, 2)),
    Piece(Type.C, Position(8, 3)),
    Piece(Type.A, Position(8, 4)),
  )
)

val example =
  """
    |#############
    |#...........#
    |###B#C#B#D###
    |  #D#C#B#A#
    |  #D#B#A#C#
    |  #A#D#C#A#
    |  #########
    |""".stripMargin.split("\n").filterNot(_.isBlank)

def printState(board: Board): Unit =
  val pieceMap = board.pieces.map(p => (p.pos, getType(p.podType))).toMap
  println("#############")
  println((0 to 11).map(x => pieceMap.getOrElse(Position(x, 0), '.')).mkString)
  println("  " + (2 to 8).map(x => pieceMap.getOrElse(Position(x, 1), '.')).mkString)
  println("  " + (2 to 8).map(x => pieceMap.getOrElse(Position(x, 2), '.')).mkString)
  println("  " + (2 to 8).map(x => pieceMap.getOrElse(Position(x, 3), '.')).mkString)
  println("  " + (2 to 8).map(x => pieceMap.getOrElse(Position(x, 4), '.')).mkString)

def getType(value: Type): Char = value match {
  case Type.A => 'A'
  case Type.B => 'B'
  case Type.C => 'C'
  case Type.D => 'D'
}