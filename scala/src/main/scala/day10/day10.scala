package day10

import scala.annotation.tailrec
import scala.compiletime.ops.int./
import scala.io.Source

def solve1(lines: List[String]): Int =
  lines.flatMap(findImbalance(_)).map(getScore).sum

def getScore(c: Char): Int = c match {
  case ')' => 3
  case ']' => 57
  case '}' => 1197
  case '>' => 25137
  case _ => throw RuntimeException(s"No score for char $c")
}

def solve2(lines: List[String]): Long =
  val withoutCorruptedLines = lines.filter(findImbalance(_).isEmpty)
  val linesWithScore = withoutCorruptedLines.map(balanceLine(_)).map(str => str.foldLeft(0L)(calculateScore))
  val foo = linesWithScore.length
  linesWithScore.sorted.drop(foo/2).head

def calculateScore(score: Long, c: Char): Long =
  score * 5 + getScore2(c)

def getScore2(c: Char): Int = c match {
  case ')' => 1
  case ']' => 2
  case '}' => 3
  case '>' => 4
  case _ => throw RuntimeException(s"No score for char $c")
}

@tailrec
def balanceLine(string: String, parsed: List[Char] = List()): List[Char] =
  if string.isEmpty then
    parsed.map(openToClose)
  else
    val head = string.head
    if isOpening(head) then
      balanceLine(string.tail, head :: parsed)
    else
      balanceLine(string.tail, parsed.tail)

@tailrec
def findImbalance(string: String, parsed: List[Char] = List()): Option[Char] =
  if string.isEmpty then
    Option.empty
  else
    val head = string.head
    if isOpening(head) then
      findImbalance(string.tail, head :: parsed)
    else if openToClose(parsed.headOption.getOrElse('f')) == head then
      findImbalance(string.tail, parsed.tail)
    else
      Option(head)

def isOpening(c: Char): Boolean =
  Set('(', '[', '{', '<').contains(c)

def isClosing(c: Char): Boolean =
  Set(')', ']', '}', '>').contains(c)

def openToClose(c: Char): Char = c match {
  case '(' => ')'
  case '[' => ']'
  case '{' => '}'
  case '<' => '>'
  case _ => throw RuntimeException(s"Char $c is not handled")
}

@main
def main(): Unit =
  val input = Source.fromResource("day10.txt").getLines().toList
  //val input = example.split("\n").toList

  println("Pt1: " + solve1(input))
  println("Pt2: " + solve2(input))


val example =
  """[({(<(())[]>[[{[]{<()<>>
    |[(()[<>])]({[<{<<[]>>(
    |{([(<{}[<>[]}>{[]{[(<()>
    |(((({<>}<{<{<>}{[]{[]{}
    |[[<[([]))<([[{}[[()]]]
    |[{[{({}]{}}([{[{{{}}([]
    |{<[[]]>}<{[{[{[]{()[[[]
    |[<(<(<(<{}))><([]([]()
    |<{([([[(<>()){}]>(<<{{
    |<{([{{}}[<[[[<>{}]]]>[]]
    |""".stripMargin