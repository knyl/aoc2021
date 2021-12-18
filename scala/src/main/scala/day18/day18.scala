package day18

import scala.annotation.tailrec
import scala.io.Source


type SnailNum = SnailNumber | Num

case class SnailNumber(l: SnailNum, r: SnailNum)

case class Num(num: Int)

val startParen = raw"^\[(.*)".r
val stopParen = raw"^](.*)".r
val pair = raw"^\[(\d+),(\d+)](.*)".r
val bigDigit = raw"^(\d\d+)(.*)".r
val isDigit = raw"^(\d+)(.*)".r

@tailrec
def findFirstPairToExplode(str: String, parsedStr: String = "", count: Int = 0): String =
  str match {
    case "" => parsedStr.reverse
    case pair(d1, d2, rest) if count >= 4 => explode(rest, parsedStr, d1.toInt, d2.toInt)
    case startParen(rest) => findFirstPairToExplode(rest, str.head + parsedStr, count + 1)
    case stopParen(rest) => findFirstPairToExplode(rest, str.head + parsedStr, count - 1)
    case _ => findFirstPairToExplode(str.tail, str.head + parsedStr, count)
  }

@tailrec
def trySplit(str: String, parsedStr: String = ""): String =
  str match {
    case "" => parsedStr.reverse
    case bigDigit(d, rest) => split(rest, parsedStr, d.toInt)
    case _ => trySplit(str.tail, str.head + parsedStr)
  }

def explode(str: String, parsedStr: String, left: Int, right: Int): String =
  val rightStr = addToString(str, right)
  val leftStr = addToLeftString(parsedStr, left)
  leftStr.reverse + "0" + rightStr


@tailrec
def addToString(str: String, i: Int, acc: String = ""): String = str match {
  case "" => acc.reverse
  case isDigit(d, rest) => acc.reverse + (d.toInt + i).toString + rest
  case _ => addToString(str.tail, i, str.head + acc)
}

@tailrec
def addToLeftString(str: String, i: Int, acc: String = ""): String = str match {
  case "" => acc.reverse
  case isDigit(d, rest) => acc.reverse + (d.reverse.toInt + i).toString.reverse + rest
  case _ => addToLeftString(str.tail, i, str.head + acc)
}

def split(str: String, parsedStr: String, digit: Int): String =
  parsedStr.reverse + "[" + Math.floor(digit / 2.0).toInt.toString + "," + Math.ceil(digit / 2.0).toInt.toString + "]" + str

@tailrec
def reduce(str: String): String =
  val newStr = findFirstPairToExplode(str)
  if newStr == str then
    val newStr2 = trySplit(newStr)
    if newStr2 == newStr then
      str
    else
      reduce(newStr2)
  else
    reduce(newStr)

@tailrec
def addUpNumbers(numbers: List[String]): String = numbers match {
  case List(n) => n
  case List(n1, n2, _*) =>
    val str = reduce("[" + reduce(n1) + "," + reduce(n2) + "]")
    addUpNumbers(str :: numbers.tail.tail)
}

def calculateMagnitude(snailNum: SnailNum): Int = snailNum match {
  case Num(n) => n
  case SnailNumber(sn1, sn2) => 3 * calculateMagnitude(sn1) + 2 * calculateMagnitude(sn2)
}

val comma = raw"^,(.*)".r

def parse(string: String): (SnailNum, String) = string match {
  case startParen(tail) =>
    val (leftNum, rest) = parse(tail)
    val (rightNum, rest2) = parse(rest)
    (SnailNumber(leftNum, rightNum), rest2)
  case isDigit(digit, rest) => (Num(digit.toInt), rest)
  case comma(rest) => parse(rest)
  case stopParen() => parse(string.tail)
  case _ => throw RuntimeException(s"Couldn't match $string")
}

@main
def main(): Unit =
  val lines = Source.fromResource("day18.txt").getLines().filterNot(_.isBlank).toList

  val str = addUpNumbers(lines)
  val (snailNum, _) = parse(str)
  val magnitude = calculateMagnitude(snailNum)
  println(s"Pt 1: $magnitude")

  val combinations = for l1 <- lines; l2 <- lines if l1 != l2 yield s"[$l1,$l2]"
  val maxMagnitude = combinations.map(reduce).map(parse).map((sn, str) => calculateMagnitude(sn)).max
  println(s"Pt 2: $maxMagnitude")
