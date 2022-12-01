package aoc

case object Day1 extends AOCProblem {

  val day: Int = 1

  private val input = scala.io.Source.fromFile("./priv/day1/input.txt").getLines().toSeq

  val caloriesPerElf: Seq[Int] =
    input.mkString("\n")
      .split("\n\n")
      .map(_.split("\n").toSeq.map(_.toInt))
      .toSeq.map(_.sum)

  def solve1: Int = {
    caloriesPerElf.max
  }

  def solve2: Int = {
    caloriesPerElf.sorted.reverse.take(3).sum
  }
}
