package day8

import scala.io.Source

case class Entry(patterns: List[String], output: List[String])

def solve1(lines: List[Entry]): Int =
  lines.map(_.output).flatMap(_.filter(s => s.length == 2 || s.length == 3 || s.length == 4 || s.length == 7)).size

def solve2(entries: List[Entry]): Int =
  val finalNumbers = entries.map(entry => (entry.output, getTranslation(entry))).map(applyTranslation)
  finalNumbers.map(decodeOutput).sum

def decodeOutput(number: List[String]): Int =
  number.map(decodeDigit).mkString.toInt

def decodeDigit(digit: String): Char = sortedNum(digit) match {
  case "abcefg" => '0'
  case "cf" => '1'
  case "acdeg" => '2'
  case "acdfg" => '3'
  case "bcdf" => '4'
  case "abdfg" => '5'
  case "abdefg" => '6'
  case "acf" => '7'
  case "abcdefg" => '8'
  case "abcdfg" => '9'
  case _ => throw RuntimeException(s"$digit can't be decoded")
}

def sortedNum(num: String): String =
  num.toList.sorted.mkString

def getTranslation(line: Entry): Map[Char, Char] = // TODO
  val patterns = line.patterns

  val char1 = findOfLength(patterns, 2)
  val char7 = findOfLength(patterns, 3)
  val char4 = findOfLength(patterns, 4)
  val char8 = findOfLength(patterns, 7)

  val a = char7.filter(!char1.contains(_)).head

  val (char9, g) = deduceChar(char4 + a, patterns.filter(_.length == 6))
  val (char3, d) = deduceChar(char7 + g, patterns.filter(_.length == 5))

  val b = char4.filter(!(char1 + d).contains(_)).head
  val e = char8.filter(!char9.contains(_)).head

  val (char2, c) = deduceChar(List(a, d, e, g).mkString, patterns.filter(_.length == 5))

  val f = char1.filter(_ != c).head
  Map(
    (a, 'a'),
    (b, 'b'),
    (c, 'c'),
    (d, 'd'),
    (e, 'e'),
    (f, 'f'),
    (g, 'g'),
  )

def findOfLength(patterns: List[String], length: Int): String = {
  patterns.find(_.length == length).orNull
}

def deduceChar(filter: String, chars: List[String]): (String, Char) =
  val num = chars.find(str => str.count(!filter.contains(_)) == 1).orNull
  (num, num.filter(!filter.contains(_)).head)

def applyTranslation(outputs: List[String], mapping: Map[Char, Char]): List[String] =
  outputs.map(toNum(_, mapping))

def toNum(str: String, mapping: Map[Char, Char]): String =
  str.flatMap(mapping.get).mkString

def parseInput(input: String): Entry = input match {
  case s"$n1 |$n2" => Entry(toNums(n1), toNums(n2))
  case _ => throw RuntimeException(s"Input: $input not matching")
}

def toNums(string: String): List[String] =
  string.stripTrailing.stripLeading.split(" ").map(sortedNum).toList

@main
def main(): Unit =
  val input = Source.fromResource("day8.txt").getLines().filterNot(_.isBlank).map(parseInput).toList
  //val input = example.split("\n").filterNot(_.isEmpty).map(parseInput).toList
  //val input = List("acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |cdfeb fcadb cdfeb cdbaf").map(parseInput)
  //val input = List("gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc |fgae cfgab fg bagce").map(parseInput)

  println("Pt1: " + solve1(input))
  println("Pt2: " + solve2(input))

val example =
  """
    |be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb |fdgacbe cefdb cefbgd gcbe
    |edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec |fcgedb cgb dgebacf gc
    |fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef |cg cg fdcagb cbg
    |fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega |efabcd cedba gadfec cb
    |aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga |gecf egdcabf bgf bfgea
    |fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf |gebdcfa ecba ca fadegcb
    |dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf |cefg dcbef fcge gbcadfe
    |bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd |ed bcgafe cdgba cbgef
    |egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg |gbdfcae bgc cg cgb
    |gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc |fgae cfgab fg bagce
    |""".stripMargin