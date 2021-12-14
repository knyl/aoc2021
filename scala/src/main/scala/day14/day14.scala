package day14

import scala.annotation.tailrec
import scala.io.Source

type MapKey = (Char, Char)
type Mappings = Map[MapKey, Char]
type PairCounts = Map[MapKey, Long]

def solve(start: String, mappings: Mappings, iterations: Int): Long =
  val pairMap = createPairMap(start)
  val resultingPairs = (0 until iterations).foldLeft(pairMap)(insertPolymers(mappings))
  getResult(resultingPairs, start)

def createPairMap(start: String): PairCounts =
  (start zip start.tail).groupBy(identity).map((k, v) => (k, v.size.toLong))

def getResult(resultingPairs: PairCounts, start: String): Long =
  val edgeChars = Set(start.charAt(0), start.charAt(start.length - 1))
  val charCounts = resultingPairs.toList
    .flatMap((k, v) => List((k._1, v), (k._2, v)))
    .groupBy(_._1)
    .map((k, v) => (k, v.map(_._2).sum))
    .map((k, v) => if edgeChars.contains(k) then (k, v + 1L) else (k, v))
  (charCounts.values.max / 2) - (charCounts.values.min / 2)

def insertPolymers(mappings: Mappings)(pairs: PairCounts, it: Int): PairCounts =
  pairs.foldLeft(Map())(insertNewPolymers(mappings))

def insertNewPolymers(mappings: Mappings)(pairs: PairCounts, el: (MapKey, Long)): PairCounts =
  val newPairs = getNewPairs(el._1, el._2, mappings)
  pairs ++ newPairs.map((k, v) => (k, v + pairs.getOrElse(k, 0L)))

def getNewPairs(key: MapKey, v: Long, mappings: Mappings): List[(MapKey, Long)] = (key, mappings.get(key)) match
  case (_, None) => List((key, v))
  case ((c1, c2), Some(char)) => List(((c1, char), v), ((char, c2), v))

def parseInput(lines: List[String]): Mappings = lines.map(getLine).toMap

def getLine(line: String): ((Char, Char), Char) = line.toCharArray match
  case Array(c1, c2, ' ', '-', '>', ' ', v) => ((c1, c2), v)
  case _ => throw RuntimeException(s"Can't parse $line")

@main
def main(): Unit =
  //val lines = Source.fromResource("day14.txt").getLines().filterNot(_.isBlank).toList
  val lines = example.split("\n").filterNot(_.isBlank).toList
  val start = lines.head
  val mappings = parseInput(lines.tail)

  println("Pt1: " + solve(start, mappings, 10))
  println("Pt2: " + solve(start, mappings, 40))


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