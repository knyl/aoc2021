package day21

import scala.io.Source
import scala.collection.mutable

case class GameState(player1: Player, player2: Player)

case class Player(position: Int, score: Int)

type State = mutable.Map[(Player, Player), Amount]
type Position = Int
type Score = Int
type Amount = Long

def getNext(die: Int, position: Int): (Int, Int) =
  val dieSum = die + reduceDie(die + 1) + reduceDie(die + 2)
  (reduceDie(die + 3), reducePos(position + dieSum))

def reduceDie(die: Int): Int =
  if die > 100 then die % 101 + 1 else die

def reducePos(pos: Int): Int =
  if pos % 10 == 0 then 10
  else if pos < 10 then pos
  else pos % 10

def generatePosToScore(): Map[Int, List[(Int, Long)]] =
  val dieSums: Map[Int, Long] = (for i <- 1 to 3; j <- 1 to 3; k <- 1 to 3 yield i + j + k)
    .groupBy(identity)
    .map((i, list) => (i, list.size.toLong))

  val data: Map[Int, List[(Int, Long)]] = (1 to 10).map(pos => (pos, dieSums.map((sum, amount) => (reducePos(pos + sum), amount)).toList)).toMap
  data

def isFinished(p1: Player, p2: Player): Boolean =
  p1.score >= 21 || p2.score >= 21

def solve2(initialState: GameState): Unit =
  val posToScore = generatePosToScore()

  var state: State = mutable.Map()
  val winnings: State = mutable.Map()
  state.addOne(((Player(initialState.player1.position, 0), Player(initialState.player2.position, 0)), 1L))

  var isPlayer1 = true
  while state.nonEmpty do {
    val newState: State = mutable.Map()
    for ((p1, p2), amount) <- state do {
      if isFinished(p1, p2) then
        winnings.addOne(((p1, p2), amount + winnings.getOrElse((p1, p2), 0L)))
      else {
        if isPlayer1 then {
          val newPosToAmount = posToScore(p1.position)
          newPosToAmount.foreach((newPos, newAmount) => {
            val newKey = (Player(newPos, newPos + p1.score), p2)
            newState.addOne((newKey, (newAmount * amount) + newState.getOrElse(newKey, 0L)))
          })
        } else {
          val newPosToAmount = posToScore(p2.position)
          newPosToAmount.foreach((newPos, newAmount) => {
            val newKey = (p1, Player(newPos, newPos + p2.score))
            newState.addOne((newKey, (newAmount * amount) + newState.getOrElse(newKey, 0L)))
          })
        }
      }
    }
    state = newState
    val universes = winnings.map((k, v) => v).sum + state.map((k, v) => v).sum
    isPlayer1 = !isPlayer1
  }

  val player1Sum = winnings.filter((k, amount) => k._1.score >= 21).map((k, amount) => amount).sum
  val player2Sum = winnings.filter((k, amount) => k._2.score >= 21).map((k, amount) => amount).sum

  println(s"Pt 2: ${Math.max(player1Sum, player2Sum)}")


@main
def main(): Unit =

  val lines = Source.fromResource("day21.txt").getLines().toList
  //val startState = GameState(Player(4, 0), Player(8, 0)) // example
  val startState = GameState(Player(8, 0), Player(3, 0)) // input

  var playing = true
  var gameState = startState
  var die = 1
  var dieRolls = 0
  while playing do {
    val (newDie1, nextPos1) = getNext(die, gameState.player1.position)
    val newScore1 = nextPos1 + gameState.player1.score
    dieRolls += 3
    if newScore1 > 999 then
      playing = false
      println(s"Pt 1: ${dieRolls * gameState.player2.score}")

    val (newDie2, nextPos2) = getNext(newDie1, gameState.player2.position)
    val newScore2 = nextPos2 + gameState.player2.score
    dieRolls += 3
    if newScore2 > 999 then
      playing = false
      println(s"Pt 1: ${dieRolls * gameState.player1.score}")
    gameState = GameState(Player(nextPos1, newScore1), Player(nextPos2, newScore2))
    die = newDie2
  }

  solve2(startState)
