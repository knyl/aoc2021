package day2

import scala.annotation.tailrec
import scala.io.Source


enum Direction:
  case FORWARD, DOWN, UP

case class Position(horizontal: Int, depth: Int, aim: Int)

case class Instruction(direction: Direction, steps: Int)


def solve(numbers: Iterable[String], directionToStep: (Position, Instruction) => Position): Int =
  val instructions = numbers.toList.map(parseInstruction)
  val finalPosition = instructions.foldLeft(Position(0, 0, 0)) { (acc: Position, el: Instruction) => directionToStep(acc, el) }
  finalPosition.horizontal * finalPosition.depth

def calculate(position: Position, instruction: Instruction): Position =
  instruction.direction match {
    case Direction.UP => Position(position.horizontal, position.depth - instruction.steps, 0)
    case Direction.DOWN => Position(position.horizontal, position.depth + instruction.steps, 0)
    case Direction.FORWARD => Position(position.horizontal + instruction.steps, position.depth, 0)
  }

def calculateWithAim(position: Position, instruction: Instruction): Position =
  instruction.direction match {
    case Direction.UP => Position(position.horizontal, position.depth, position.aim - instruction.steps)
    case Direction.DOWN => Position(position.horizontal, position.depth, position.aim + instruction.steps)
    case Direction.FORWARD => Position(position.horizontal + instruction.steps, position.depth + position.aim * instruction.steps, position.aim)
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
  val example = List("forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2")
  println(solve(example, calculate) + " " + solve(example, calculateWithAim))
  val lines = Source.fromResource("day2.txt").getLines().toList

  println("Pt1: " + solve(lines, calculate))
  println("Pt2: " + solve(lines, calculateWithAim))
