package aoc

case object Day4 extends AOCProblem[Int] {
  val day = 4

  private val input = scala.io.Source.fromFile("./priv/day4/input.txt").getLines().toSeq

  val pattern = "(\\d+)-(\\d+),(\\d+)-(\\d+)".r


  def solve1: Int = {
    input.map { line =>
      val pattern(s1, e1, s2, e2) = line
      val r1 = Range.inclusive(s1.toInt, e1.toInt)
      val r2 = Range.inclusive(s2.toInt, e2.toInt)
      if (r1.length >= r2.length) {
        if (r1.containsSlice(r2)) 1 else 0
      } else {
        if (r2.containsSlice(r1)) 1 else 0
      }
    }.sum
  }

  def solve2: Int = {
    input.map { line =>
      val pattern(s1, e1, s2, e2) = line
      val r1 = Range.inclusive(s1.toInt, e1.toInt).toSet
      val r2 = Range.inclusive(s2.toInt, e2.toInt).toSet
      if (r1.intersect(r2).nonEmpty) 1 else 0
    }.sum
  }
}
