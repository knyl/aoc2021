package day2

import scala.annotation.tailrec
import scala.io.Source


enum Direction:
  case FORWARD, DOWN, UP

case class Position(horizontal: Int, depth: Int)

case class Instruction(direction: Direction, steps: Int)


def solve2(numbers: Iterable[String]): Int =
  val instructions = numbers.toList.map(parseInstruction)
  val result = calculate(instructions)
  result.horizontal * result.depth

def calculate(instructions: List[Instruction], position: Position = Position(0, 0)): Position =
  if (instructions.isEmpty)
    position
  else
    instructions.head match {
      case Instruction(direction, steps) if direction == Direction.UP => calculate(instructions.tail, Position(position.horizontal, position.depth - steps))
      case Instruction(direction, steps) if direction == Direction.DOWN => calculate(instructions.tail, Position(position.horizontal, position.depth + steps))
      case Instruction(direction, steps) if direction == Direction.FORWARD => calculate(instructions.tail, Position(position.horizontal + steps, position.depth))
      case _ => throw new RuntimeException("shouldn't happen")
    }

def parseInstruction(instruction: String): Instruction =
  val forwardMatch = raw"forward (\d*)".r
  val downMatch = raw"down (\d*)".r
  val upMatch = raw"up (\d*)".r

  instruction match {
    case forwardMatch(steps) => Instruction(Direction.FORWARD, steps.toInt)
    case downMatch(steps) => Instruction(Direction.DOWN, steps.toInt)
    case upMatch(steps) => Instruction(Direction.UP, steps.toInt)
    case _ => throw new RuntimeException("shouldn't happen")
  }


@main
def main(): Unit =
  val lines = Source.fromResource("day2.txt").getLines().toList

  println("Pt1: " + solve2(lines))
