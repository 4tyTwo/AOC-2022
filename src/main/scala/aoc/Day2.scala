package aoc

case object Day2 extends AOCProblem {
  val day = 2

  private val input = scala.io.Source.fromFile("./priv/day2/input.txt").getLines().toSeq

  sealed trait Outcome {
    val points: Int
  }

  object Outcome {
    def apply(char: Char): Outcome = {
      char match {
        case 'X' => Loss
        case 'Y' => Draw
        case 'Z' => Win
      }
    }
  }

  case object Loss extends Outcome { val points = 0 }
  case object Draw extends Outcome { val points = 3 }
  case object Win  extends Outcome { val points = 6 }

  sealed trait RPS {
    val points: Int
    def playAgainst(other: RPS): Outcome
  }

  object RPS {
    def apply(char: Char): RPS = {
      char match {
        case c if c == 'A' || c == 'X' => Rock
        case c if c == 'B' || c == 'Y' => Paper
        case c if c == 'C' || c == 'Z' => Scissors
      }
    }
  }

  case object Rock extends RPS {
    val points = 1
    def playAgainst(other: RPS): Outcome = other match {
      case Rock => Draw
      case Paper => Loss
      case Scissors => Win
    }
  }
  case object Paper extends RPS {
    val points = 2
    def playAgainst(other: RPS): Outcome = other match {
      case Rock => Win
      case Paper => Draw
      case Scissors => Loss
    }
  }
  case object Scissors extends RPS {
    val points = 3
    def playAgainst(other: RPS): Outcome = other match {
      case Rock => Loss
      case Paper => Win
      case Scissors => Draw
    }
  }

  def createLookUpTable: Map[(RPS, Outcome), RPS] = {
    Seq(Rock, Paper, Scissors).foldLeft(Map.empty[(RPS, Outcome), RPS]) ( (m1, rps1) => {
      Seq(Rock, Paper, Scissors).foldLeft(m1)((m2, rps2) => {
        val o = rps1.playAgainst(rps2)
        m2 + ((rps2, o) -> rps1)
      })
    })
  }

  override def solve1: Int = {
    input.map { line =>
      val a = RPS(line(0))
      val b = RPS(line(2))
      b.points + b.playAgainst(a).points
    }.sum
  }

  override def solve2: Int = {
    val lookup = createLookUpTable
    input.map { line =>
      val a = RPS(line(0))
      val o = Outcome(line(2))
      val b = lookup((a, o))
      b.points + o.points
    }.sum
  }
}
