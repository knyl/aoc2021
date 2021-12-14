package day14

import scala.annotation.tailrec
import scala.io.Source


def solve(start: String, mappings: Map[String, String]): Int =
  val lastString = doIterations(start, mappings, 10)
  val groups = lastString.toCharArray.groupBy(identity).values.map(_.length)
  groups.max - groups.min

@tailrec
def doIterations(start: String, mappings: Map[String, String], iterations: Int, currIteration: Int = 0): String =
  if currIteration == iterations then
    start.reverse
  else
    val (acc, newString) = start.tail.foldLeft((mappings, start.head.toString))(nextString)
    //println(s"$currIteration: ${newString.reverse}")
    doIterations(newString.reverse, mappings, iterations, currIteration + 1)

def nextString(acc: (Map[String, String], String), el: Char): (Map[String, String], String) =
  val (mappings, currString) = acc
  val key = currString.head.toString + el.toString
  val value = mappings.getOrElse(key, "")
  val nextString = el.toString + value + currString
  (mappings, nextString)

def solve2(start: String, mappings: Map[String, String]): Long =
  val pairs: List[String] = (start.tail zip start.reverse.tail.reverse).map((c1, c2) => c2.toString + c1.toString).toList
  val pairMap: Map[String, Long] = pairs.groupBy(identity).map((k, v) => (k, v.size.toLong))
  val resultingPairs = doIterations2(pairMap, mappings, 40)
  val charCounts: List[(String, Long)] = resultingPairs.toList.flatMap((k:String, v:Long) => List((k.charAt(0).toString, v), (k.charAt(1).toString, v)))
  val finalCharCounts0: Map[String, Long] = charCounts.groupBy(_._1).map((k:String, v:List[(String, Long)]) => (k, v.map(_._2).sum))
  val startChar = start.charAt(0).toString
  val endChar = start.reverse.charAt(0).toString
  val finalCharCounts = finalCharCounts0.map((k, v) => if k == startChar || k == endChar then (k, v+1) else (k, v))
  (finalCharCounts.values.max / 2) - (finalCharCounts.values.min / 2)

@tailrec
def doIterations2(pairs: Map[String, Long], mappings: Map[String, String], iterations: Long, currIteration: Long = 0): Map[String, Long] =
  if iterations == currIteration then
    pairs
  else
   val pairList: List[(String, Long)] = pairs.toList.flatMap((k, v) => getNewPairs(k, v, mappings))
   val groups0 = pairList.groupBy(_._1)
   val groups = groups0.map((key, list) => (key, list.map(_._2).sum)).toMap
   doIterations2(groups, mappings, iterations, currIteration + 1)

def getNewPairs(key: String, v: Long, mappings: Map[String, String]): List[(String, Long)] =
  if mappings.contains(key) then
    val newChar = mappings.getOrElse(key, "")
    List((key.charAt(0).toString + newChar, v), (newChar + key.charAt(1).toString, v))
  else
    List((key, v))

def parseInput(lines: List[String]): Map[String, String] = lines.map(getLine).toMap
def getLine(line: String): (String, String) = line match {
  case s"$k -> $v" => (k, v)
  case _ => throw RuntimeException(s"Can't parse $line")
}

@main
def main(): Unit =
  val lines = Source.fromResource("day14.txt").getLines().filterNot(_.isBlank).toList
  //val lines = example.split("\n").filterNot(_.isBlank).toList
  val start = lines.head
  val mappings = parseInput(lines.tail)

  println("Pt1: " + solve(start, mappings))
  println("Pt2: " + solve2(start, mappings))


val example =
  """
    |NNCB
    |
    |CH -> B
    |HH -> N
    |CB -> H
    |NH -> C
    |HB -> C
    |HC -> B
    |HN -> C
    |NN -> C
    |BH -> H
    |NC -> B
    |NB -> B
    |BN -> B
    |BB -> N
    |BC -> B
    |CC -> N
    |CN -> C
    |""".stripMargin