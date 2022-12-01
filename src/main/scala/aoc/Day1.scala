package aoc

case object Day1 extends AOCProblem {

  val day: Int = 1

  private val input = scala.io.Source.fromFile("./priv/day1/input.txt").getLines().toSeq

  def caloriesPerElf: Seq[Int] =
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

case object Day1Alt extends AOCProblem {
  // More efficient, less scala-like
  val day = 1
  private val input = scala.io.Source.fromFile("./priv/day1/input.txt").getLines().toSeq

  def createCaloriesPQ: collection.mutable.PriorityQueue[Int] = {
    val pq = collection.mutable.PriorityQueue.empty[Int]
    var acc = 0
    input.foreach( s =>
      if (s.isBlank) {
        pq.addOne(acc)
        acc = 0
      } else {
        acc += s.toInt
      }
    )
    pq
  }

  def solve1: Int = {
    createCaloriesPQ.dequeue()
  }

  def solve2: Int = {
    createCaloriesPQ.dequeueAll.take(3).sum
  }
}