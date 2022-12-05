package aoc

import scala.collection.mutable
import scala.util.matching.Regex

case object Day5 extends AOCProblem[String] {
  val day = 5

  private val input = scala.io.Source.fromFile("./priv/day5/input.txt").getLines().mkString("\n")

  private val parts = input.split("\n\n").toSeq
  private val cratesDescription = parts.head.split("\n").toSeq.reverse

  case class Instruction(howMany: Int, from: Int, to: Int)

  object Instruction {
    private val pattern = "move (\\d+) from (\\d+) to (\\d+)".r

    def apply(s: String): Instruction = {
      val pattern(howMany, from ,to) = s
      Instruction(howMany.toInt, from.toInt, to.toInt)
    }
  }

  def executeInstruction9000(crates: Array[mutable.Stack[Char]], instruction: Instruction) = {
    executeInstruction(crates, instruction, false)
  }

  def executeInstruction9001(crates: Array[mutable.Stack[Char]], instruction: Instruction) = {
    executeInstruction(crates, instruction, true)
  }

  def executeInstruction(crates: Array[mutable.Stack[Char]], instruction: Instruction, reverse: Boolean) = {
    val toMove = (1 to instruction.howMany).map { _ =>
      crates(instruction.from - 1).pop()
    }
    crates(instruction.to - 1).pushAll(if (reverse) toMove.reverse else toMove)
  }

  val instructions: Seq[Instruction] = parts(1).split("\n").map(Instruction(_)).toSeq

  def createCrates: Array[mutable.Stack[Char]] = {
    val crates: Array[mutable.Stack[Char]] = cratesDescription.head.trim.split("   ").map(_  => mutable.Stack.empty[Char])
    cratesDescription.tail.map { line =>
      line.indices.map { i =>
        val c = line(i)
        if (c >= 'A' && c <= 'Z') {
          val idx = (i-1) / 4
          crates(idx).push(c)
        }
      }
    }
    crates
  }

  def solve1: String = {
    val crates = createCrates
    instructions.foreach(executeInstruction9000(crates, _))
    crates.map(_.pop()).mkString("")
  }

  def solve2: String = {
    val crates = createCrates
    instructions.foreach(executeInstruction9001(crates, _))
    crates.map(_.pop()).mkString("")
  }
}
