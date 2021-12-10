package day10

import scala.annotation.tailrec
import scala.compiletime.ops.int./
import scala.io.Source

enum ResultType:
  case INCOMPLETE, CORRUPT

case class Result(chars: List[Char], resultType: ResultType)

def getScore(c: Char): Int = c match {
  case ')' => 3
  case ']' => 57
  case '}' => 1197
  case '>' => 25137
  case _ => throw RuntimeException(s"No score for char $c")
}

def solve(lines: List[String]): (Long, Long) =
  val lineResult = lines.map(getMissingChars(_))
  val corruptedLines = lineResult.filter(_.resultType == ResultType.CORRUPT)
  val incompleteLines = lineResult.filter(_.resultType == ResultType.INCOMPLETE)
  val scoreDay1 = corruptedLines.map(_.chars.head).map(getScore).sum
  val scoreDay2 = incompleteLines.map(_.chars).map(_.foldLeft(0L)(calculateScore2)).sorted.drop(incompleteLines.length/2).head
  (scoreDay1, scoreDay2)

def calculateScore2(score: Long, c: Char): Long =
  score * 5 + getScore2(c)

def getScore2(c: Char): Int = c match {
  case ')' => 1
  case ']' => 2
  case '}' => 3
  case '>' => 4
  case _ => throw RuntimeException(s"No score for char $c")
}

@tailrec
def getMissingChars(string: String, parsed: List[Char] = List()): Result =
  if string.isEmpty then
    Result(parsed.map(openToClose), ResultType.INCOMPLETE)
  else
    val head = string.head
    if isOpening(head) then
      getMissingChars(string.tail, head :: parsed)
    else if openToClose(parsed.headOption.getOrElse('f')) == head then
      getMissingChars(string.tail, parsed.tail)
    else
      Result(List(head), ResultType.CORRUPT)

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
  val (day1, day2) = solve(input)

  println("Pt1: " + day1)
  println("Pt2: " + day2)


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