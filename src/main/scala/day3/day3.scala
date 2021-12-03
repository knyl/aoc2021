package day3

import scala.io.Source

def solve(numbers: Iterable[String]): Int =
  val res = numbers.foldLeft(List.fill(numbers.head.size)(0)) { (acc, el) => count_elements(acc, el) }
  println(res)
  toBinary(res)

def toBinary(value: List[Int]): Int =
  val string = value.map(c => {
    if c.toInt > 0 then '1' else '0'
  }).mkString
  val inverseString = string.map(c => {
    if c == '0' then '1' else '0'
  }).mkString
  val gammaRate = Integer.parseInt(string, 2)
  val epsilonRate = Integer.parseInt(inverseString, 2)
  println("Gamma rate: " + gammaRate)
  println("Epsilon rate: " + epsilonRate)
  gammaRate * epsilonRate


def count_elements(acc: List[Int], str: String): List[Int] =
  acc zip str.toList map { (a, c) => a + count(c) }


def count(pos: Char): Int = pos match
  case '0' => -1
  case '1' => 1
  case _ => throw RuntimeException("should never happen: " + pos)

def solve2(numbers: Iterable[String]): Int =
  val oxygenRating = getRating(numbers.toList, oxygenGeneratorRatingFun)
  println("Oxygen: " + oxygenRating)
  val co2Rating = getRating(numbers.toList, co2ScrubberRatingFun)
  println("co2: " + co2Rating)
  oxygenRating * co2Rating

def getRating(numbers: List[String], selectFun: (Iterable[Char]) => Char): Int =
  var finalList = List.from(numbers)
  val length = numbers.head.length
  var prevI = 0
  while (finalList.size > 1)
    for i <- (prevI until length)
        if finalList.size > 1
    do
      val l = finalList.map(_.charAt(i))
      val c = selectFun(l)
      finalList = finalList.filter(_.charAt(i) == c)
    prevI = prevI + 1

  val foo = finalList
  Integer.parseInt(finalList.head, 2)

def oxygenGeneratorRatingFun(chars: Iterable[Char]): Char =
  val zeroCount = chars.count(_ == '0')
  val oneCount = chars.count(_ == '1')
  if zeroCount > oneCount then '0' else '1'

def co2ScrubberRatingFun(chars: Iterable[Char]): Char =
  val zeroCount = chars.count(_ == '0')
  val oneCount = chars.count(_ == '1')
  if zeroCount <= oneCount then '0' else '1'


@main
def main(): Unit =
  val example = List("00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010")
  println(example)
  println("Ex: " + solve2(example))
  val lines = Source.fromResource("day3.txt").getLines().toList

  println("Pt1: " + solve(lines))
  println("Pt2: " + solve2(lines))
