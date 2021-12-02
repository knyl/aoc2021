import scala.annotation.tailrec
import scala.io.Source

@tailrec
def solve(numbers: List[Int], prev: Int = Int.MaxValue, acc: Int = 0) : Int =
  numbers match {
    case Nil => acc
    case n1 :: tail if n1 > prev => solve(tail, n1, acc+1)
    case _ => solve(numbers.tail, numbers.head, acc)
  }

def consecutiveSumsOfThree(numbers: List[Int]): List[Int] =
  numbers.sliding(3).map(_.sum).toList

@main
def main(): Unit =
  val lines = Source.fromResource("day1.txt").getLines().map(_.toInt).toList

  println("Pt1: " + solve(lines))
  println("Pt2: " + solve(consecutiveSumsOfThree(lines)))
