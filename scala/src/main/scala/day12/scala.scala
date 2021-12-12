package day12

import scala.io.Source

type Graph = Map[String, List[String]]

def solve(graph: Graph): Int =
  val paths = dfs("start", graph)
  paths.size

def dfs(node: String, graph: Graph, visited: Set[String] = Set(), path: List[String] = List()): List[List[String]] =
  if node == "end" then
    List((node :: path).reverse)
  else
    val neighbours = graph.getOrElse(node, List())
    neighbours.filter(canVisit(_, visited)).flatMap(dfs(_, graph, visited + node, node :: path))

def canVisit(node: String, visited: Set[String]): Boolean =
  !visited.contains(node) || (node == node.toUpperCase)

def parseInput(lines: List[String]): Graph =
  lines.foldLeft(Map())(addToGraph)

def parseInput(string: String): (String, String) = string match {
  case s"$n1-$n2" => (n1, n2)
  case _ => throw RuntimeException(s"Can't parse input: $string")
}

def addToGraph(graph: Graph, string: String): Graph =
  val (node1, node2) = parseInput(string)
  val edge1 = node2 :: graph.getOrElse(node1, List())
  val edge2 = node1 :: graph.getOrElse(node2, List())
  graph ++ List((node1, edge1), (node2, edge2))

@main
def main(): Unit =
  val input = Source.fromResource("day12.txt").getLines().toList
  val ex1 = example1.split("\n").toList
  val ex2 = example2.split("\n").toList
  val ex3 = example3.split("\n").toList
  println("Ex1: " + solve(parseInput(ex1)))
  println("Ex2: " + solve(parseInput(ex2)))
  println("Ex3: " + solve(parseInput(ex3)))

  println("Pt1: " + solve(parseInput(input)))
  //println("Pt2: " + solve2(map))


val example1 =
  """|start-A
    |start-b
    |A-c
    |A-b
    |b-d
    |A-end
    |b-end
    |""".stripMargin

val example2 =
  """|dc-end
    |HN-start
    |start-kj
    |dc-start
    |dc-HN
    |LN-dc
    |HN-end
    |kj-sa
    |kj-HN
    |kj-dc
    |""".stripMargin

val example3 =
  """|fs-end
    |he-DX
    |fs-he
    |start-DX
    |pj-DX
    |end-zg
    |zg-sl
    |zg-pj
    |pj-he
    |RW-he
    |fs-DX
    |pj-RW
    |zg-RW
    |start-pj
    |he-WI
    |zg-he
    |pj-fs
    |start-RW
    |""".stripMargin