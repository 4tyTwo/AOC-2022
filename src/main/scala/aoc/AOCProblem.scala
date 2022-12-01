package aoc

trait AOCProblem {

  val day: Int

  def solve1: Int
  def solve2: Int

  def printAnswers: Unit = {
    println(s"Day $day part 1: $solve1")
    println(s"Day $day part 2: $solve2")
  }
}
