import scala.io.Source

def solve(lines: List[String]) : String =
  lines.foreach(println)
  "foo"


@main
def main(): Unit =
  val lines = Source.fromResource("day1.txt").getLines()

  println("Pt1: $solv")
