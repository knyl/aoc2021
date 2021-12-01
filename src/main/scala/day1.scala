import scala.io.Source

def solve(numbers: List[Int], prev: Int = Int.MaxValue, acc: Int = 0) : Int =
  if numbers.isEmpty then
    acc
  else if numbers.head > prev then
    solve(numbers.tail, numbers.head, acc+1)
  else
    solve(numbers.tail, numbers.head, acc)

def consecutive(numbers: List[Int]): List[Int] =
  if numbers.size < 3 then
    List.empty
  else
    List(numbers.take(3).sum) ++ consecutive(numbers.drop(1))


@main
def main(): Unit =
  val lines = Source.fromResource("day1.txt").getLines().map(_.toInt).toList

  println(solve(consecutive(List(199,200, 208, 210, 200, 207, 240, 269, 260, 263))))
  println("Pt1: " + solve(lines))
  println("Pt2: " + solve(consecutive(lines)))
