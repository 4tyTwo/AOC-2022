package aoc

case object Day3 extends AOCProblem[Int] {
  val day = 3

  private val input = scala.io.Source.fromFile("./priv/day3/input.txt").getLines().toSeq

  private val fullCharSet = ('a' to 'z').toSet ++ ('A' to 'Z').toSet

  def priority(char: Char): Int = {
    char match {
      case c if c.isLower => c.toInt - 96
      case c if c.isUpper => c.toInt - 38
    }
  }


  def solve1: Int = {
    input.map { line =>
      val l = line.length
      val firstCharSet = line.substring(0, l/2).toCharArray.toSet
      val secondCharSet = line.substring(l/2).toCharArray.toSet
      priority(firstCharSet.intersect(secondCharSet).head)
    }.sum
  }

  def solve2: Int = {
    input.grouped(3).map { group =>
      priority(group.map(_.toCharArray.toSet).fold(fullCharSet) { (a, b) =>a.intersect(b)}.head)
    }.sum
  }
}
