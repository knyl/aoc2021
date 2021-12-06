package day3

import scala.annotation.tailrec
import scala.io.Source

def solve(numbers: Iterable[String]): Int =
  val initialAccumulator = List.fill(numbers.head.length)(0)
  val res = numbers.foldLeft(initialAccumulator) { (acc, el) => count_elements(acc, el) }
  toBinary(res)

def count_elements(currentCount: List[Int], nextNumber: String): List[Int] =
  currentCount zip nextNumber.toList map { (a, c) => a + increaseOrDecrease(c) }

def increaseOrDecrease(pos: Char): Int = pos match
  case '0' => -1
  case '1' => 1
  case _ => throw RuntimeException("should never happen: " + pos)

def toBinary(value: List[Int]): Int =
  val string = value.map(c => {
    if c > 0 then '1' else '0'
  }).mkString
  val inverseString = string.map(c => {
    if c == '0' then '1' else '0'
  }).mkString
  val gammaRate = Integer.parseInt(string, 2)
  val epsilonRate = Integer.parseInt(inverseString, 2)
  gammaRate * epsilonRate


def solve2(numbers: List[String]): Int =
  val oxygenRating = getRating(numbers, oxygenGeneratorRatingFun)
  val co2Rating = getRating(numbers, co2ScrubberRatingFun)
  oxygenRating * co2Rating

@tailrec
def getRating(numbers: List[String], ratingFun: Iterable[Char] => Char, currPos: Int = 0): Int = numbers match {
  case List(_) => Integer.parseInt(numbers.head, 2)
  case _ =>
    val mostCommonBit = ratingFun(numbers.map(_ (currPos)))
    getRating(numbers.filter(_ (currPos) == mostCommonBit), ratingFun, currPos + 1)
}

def oxygenGeneratorRatingFun(chars: Iterable[Char]): Char =
  val zeroCount = chars.count(_ == '0')
  if zeroCount > (chars.size - zeroCount) then '0' else '1'

def co2ScrubberRatingFun(chars: Iterable[Char]): Char =
  val zeroCount = chars.count(_ == '0')
  if zeroCount <= (chars.size - zeroCount) then '0' else '1'


@main
def main(): Unit =
  val example = List("00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010")
  println("Ex: " + solve2(example))
  val lines = Source.fromResource("day3.txt").getLines().toList

  println("Pt1: " + solve(lines))
  println("Pt2: " + solve2(lines))
