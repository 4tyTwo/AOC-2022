package aoc

import scala.collection.immutable.Queue

case object Day6 extends AOCProblem[Int] {
  val day = 6

  private val input = scala.io.Source.fromFile("./priv/day6/input.txt").getLines().toSeq.head // Only 1 line

  case class Acc(solution: Option[Int], lastN: Queue[Char], idx: Int)

  def findFirstNUniqueSequenceEnd(input: String, n: Int): Option[Int] = {
    val init = Queue.from(input.substring(0, n).toCharArray)
    input.substring(n).foldLeft(Acc(None, init, n)) { (acc, c) => acc match {
      case a @ Acc(Some(_), _, _) => a
      case Acc(_, lastN, idx) =>
        if (lastN.toSet.size == n) {
          Acc(Some(idx), lastN, idx)
        } else {
          val (_, upd1) = lastN.dequeue
          Acc(None, upd1.enqueue(c), idx + 1)
        }
    }
    }.solution
  }

  def solve1: Int = {
    findFirstNUniqueSequenceEnd(input, 4).get
  }

  def solve2: Int = {
    findFirstNUniqueSequenceEnd(input, 14).get
  }
}
