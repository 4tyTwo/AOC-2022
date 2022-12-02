package aoc

object Main extends App {

  val problems: Seq[AOCProblem] = Seq(
    Day1,
    Day2
  )
  problems.foreach(_.printAnswers)
}
