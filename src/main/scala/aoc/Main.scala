package aoc

object Main extends App {

  val problems: Seq[AOCProblem[_]] = Seq(
    Day1,
    Day2,
    Day3,
    Day4,
    Day5
  )
  problems.foreach(_.printAnswers)
}
