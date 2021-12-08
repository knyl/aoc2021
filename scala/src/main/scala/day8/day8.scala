package day8

import scala.io.Source

case class Numbers(d1: List[String], d2: List[String])

def solve1(lines: List[Numbers]): Int =
  lines.map(_.d2).flatMap(_.filter(s => s.length == 2 || s.length == 3 || s.length == 4 || s.length == 7)).size

def solve2(lines: List[Numbers]): Int =
  val mappings = lines.map(getMapping)
  val finalNumbers: List[List[String]] = (lines.map(_.d2) zip mappings) map((outputs, mapping) => applyMapping(outputs, mapping))
  finalNumbers.map(decode).sum

def decode(number: List[String]): Int =
  number.map(decode).mkString.toInt

def decode(num: String): Char =
  sortedNum(num) match {
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
  case _ => throw RuntimeException(s"$num can't be decoded")
}

def sortedNum(num: String): String =
  num.toList.sorted.mkString

def getMapping(line: Numbers): Map[Char, Char] = // TODO
  val input = line.d1
  val char1 = line.d1.find(_.length == 2).orNull
  val char7 = line.d1.find(_.length == 3).orNull
  val char4 = line.d1.find(_.length == 4).orNull
  val char8 = line.d1.find(_.length == 7).orNull
  val a = char7.filter(!char1.contains(_)).head
  val (char9, g) = deduceChar(char4 + a, line.d1.filter(_.length == 6))
  val (char3, d) = deduceChar(char7 + g, line.d1.filter(_.length == 5))
  val b = char4.filter(!(char1 + d).contains(_)).head
  val e = char8.filter(!char9.contains(_)).head
  val (char2, c) = deduceChar(List(a, d, e, g).mkString, line.d1.filter(_.length == 5))
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

def deduceChar(filter: String, chars: List[String]): (String, Char) =
  val num = chars.find(str => str.count(!filter.contains(_)) == 1).orNull
  (num, num.filter(!filter.contains(_)).head)

def applyMapping(outputs: List[String], mapping: Map[Char, Char]): List[String] =
  outputs.map(toNum(_, mapping))

def toNum(str: String, mapping: Map[Char, Char]): String =
  str.flatMap(mapping.get).mkString

def parseInput(input: String): Numbers = input match {
  case s"$n1 |$n2" => Numbers(toNums(n1), toNums(n2))
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