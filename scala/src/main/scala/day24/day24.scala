package day24

import day22.Instruction

import java.time.{LocalDateTime, ZoneOffset}
import scala.annotation.tailrec
import scala.io.Source

type Cache = Map[String, State]
type Program = Map[Int, Operations]
type State = Map[InputVariable, Int]
type Operations = List[Operation]
type InputVariable = Char
type InputOrValue = InputVariable | Int
type Operation = Inp | Add | Mul | Div | Mod | Eql

case class Inp(a: InputVariable)

case class Add(a: InputVariable, b: InputOrValue)

case class Mul(a: InputVariable, b: InputOrValue)

case class Div(a: InputVariable, b: InputOrValue)

case class Mod(a: InputVariable, b: InputOrValue)

case class Eql(a: InputVariable, b: InputOrValue)

case class ALU(variables: State, inputs: List[Int])

def isVariableOrValue(c: Char): Boolean =
  c >= '0' && c <= '9' || c == 'w' || c == 'x' || c == 'y' || c == 'z'

def parseInput(line: String): Operation = line match {
  case s"inp $a" => Inp(a.head)
  case s"add $a $b" => Add(a.head, toVariableOrValue(b))
  case s"mul $a $b" => Mul(a.head, toVariableOrValue(b))
  case s"div $a $b" => Div(a.head, toVariableOrValue(b))
  case s"mod $a $b" => Mod(a.head, toVariableOrValue(b))
  case s"eql $a $b" => Eql(a.head, toVariableOrValue(b))
  case _ => throw RuntimeException(s"Invalid input: $line")
}

def toVariableOrValue(str: String): InputOrValue =
  if str.matches("w|x|y|z") then str.head
  else str.toInt

def getValue(value: InputOrValue, variables: Map[InputVariable, Int]): Int = value match {
  case v: Char => variables(v)
  case v: Int => v
}

def doInstruction(alu: ALU, instruction: Operation): ALU = instruction match {
  case Inp(a) => ALU(alu.variables + (a -> alu.inputs.head), alu.inputs.tail)
  case Add(a, b) => ALU(alu.variables + (a -> (alu.variables(a) + getValue(b, alu.variables))), alu.inputs)
  case Mul(a, b) => ALU(alu.variables + (a -> (alu.variables(a) * getValue(b, alu.variables))), alu.inputs)
  case Div(a, b) => ALU(alu.variables + (a -> (alu.variables(a) / getValue(b, alu.variables))), alu.inputs)
  case Mod(a, b) => ALU(alu.variables + (a -> (alu.variables(a) % getValue(b, alu.variables))), alu.inputs)
  case Eql(a, b) => ALU(alu.variables + (a -> (if alu.variables(a) == getValue(b, alu.variables) then 1 else 0)), alu.inputs)
}

@tailrec
def buildCache(program: Program, cache: Cache, isMax: Boolean, pos: Int = 0): Cache =
  if pos == 13 then cache
  else
    val newCache = buildForPos(program, cache, isMax)
    buildCache(program, newCache, isMax, pos + 1)

def buildForPos(program: Program, cache: Cache, isMax: Boolean = false): Cache =
  val cacheList: List[(String, State)] = cache.toList.flatMap((num, state) => generateFor(program, state, num))
  val cacheGrouped: Map[Int, List[(String, State)]] = cacheList.groupBy((string: String, state: State) => state('z'))
  cacheGrouped.map((k, list) => getMaxOrMin(list, isMax)).toMap

def getMaxOrMin(list: List[(String, State)], isMax: Boolean = true): (String, State) =
  if isMax then
    list.maxBy(_._1)
  else
    list.minBy(_._1)

def generateFor(program: Program, state: State, num: String): List[(String, State)] =
  val currPos = num.length
  val programPart = program(currPos)
  (1 to 9).toList.map(i => (i.toString + num, programPart.foldLeft(ALU(state, List(i)))(doInstruction).variables))

@tailrec
def divideProgramIntoParts(program: Operations, acc: Program = Map(), next: Int = 0): Program =
  if program.isEmpty then acc
  else
    val nextSection = program.head :: program.tail.takeWhile(isNotInput)
    divideProgramIntoParts(program.drop(nextSection.size), acc + (next -> nextSection), next + 1)

def isNotInput(op: Operation): Boolean = op match {
  case Inp(o) => false
  case _ => true
}

def generateForMaxOrMin(program: Program, state: State, num: String, isMax: Boolean = true): Option[String] =
  val currPos = num.length
  val programPart = program(currPos)
  val values = (1 to 9).toList.map(i => (i.toString + num, programPart.foldLeft(ALU(state, List(i)))(doInstruction).variables))
  val values2: List[String] = values.filter((num, state) => state('z') == 0).map(_._1)
  if isMax then
    values2.maxOption
  else
    values2.minOption

def findValid(program: Program, cache: Cache, isMax: Boolean = true): String =
  val result = cache.toList.flatMap((num, state) => generateForMaxOrMin(program, state, num, isMax))
  if isMax then
    result.max.reverse
  else
    result.min.reverse


@main
def main(): Unit =
  val lines = Source.fromResource("day24.txt").getLines().filterNot(_.isBlank).toList

  val operations = lines.map(parseInput)
  val initialState = List('w', 'x', 'y', 'z').map((_, 0)).toMap
  val program = divideProgramIntoParts(operations)

  val maxCache = buildCache(program, Map(("", initialState)), true)
  val result = findValid(program, maxCache)
  println(s"Pt 1: $result")

  val minCache = buildCache(program, Map(("", initialState)), false)
  val result2 = findValid(program, minCache, false)
  println(s"Pt 2: $result2")
