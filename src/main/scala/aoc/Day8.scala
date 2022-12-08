package aoc

import scala.util.control.Breaks.break

case object Day8 extends AOCProblem[Int] {
  val day = 8

  private val input = scala.io.Source.fromFile("./priv/day8/input.txt").getLines().toArray

  val forest: Array[Array[Int]] = input.map(_.map(_.asDigit).toArray)

  // 0 - top to down
  // 1 - right to left
  // 2 - down to top
  // 3 - left to right
  def viewFrom(forest: Array[Array[Int]], from: Int): Array[Array[Int]] = {
    val minHeightToBeVisible = Array.ofDim[Int](forest.length, forest.head.length)
    val rowsToIterate = if (from == 2) forest.indices.reverse else forest.indices
    val colsToIterate = if (from == 1) forest.head.indices.reverse else forest.head.indices
    var prev = 0
    if (from == 1 || from == 3) {
      rowsToIterate.foreach { i =>
        colsToIterate.foreach { j =>
          val v = forest(i)(j)
          if (j != colsToIterate.head) {
            minHeightToBeVisible.update(i, minHeightToBeVisible(i).updated(j, math.max(prev + 1, v)))
          }
          prev = math.max(v, prev)
        }
        prev = 0
      }
    } else {
      colsToIterate.foreach { j =>
        rowsToIterate.foreach { i =>
          val v = forest(i)(j)
          if (i != rowsToIterate.head) {
            minHeightToBeVisible.update(i, minHeightToBeVisible(i).updated(j, math.max(prev + 1, v)))
          }
          prev = math.max(v, prev)
        }
        prev = 0
      }
    }
    minHeightToBeVisible
  }

  def solve1: Int = {
    val minHeightsSeq = (0 to 3).map(i => viewFrom(forest, i))
    val minHeights = minHeightsSeq.tail.fold(minHeightsSeq.head) { (acc, other) =>
      acc.indices.map { i =>
        acc(i).indices.map { j =>
          math.min(acc(i)(j), other(i)(j))
        }.toArray
      }.toArray
    }

    var count = 0
    forest.indices.map { i =>
      forest(i).indices.map { j =>
        if (forest(i)(j) >= minHeights(i)(j)){
          count = count + 1
        }
      }
    }
    count
  }

  // 0 - top to down
  // 1 - right to left
  // 2 - down to top
  // 3 - left to right
  def scenicScores(forest: Array[Array[Int]], from: Int): Array[Array[Int]] = {
    val scenicScores = Array.ofDim[Int](forest.length, forest.head.length)
    val rowsToIterate = if (from == 2) forest.indices.reverse else forest.indices
    val colsToIterate = if (from == 1) forest.head.indices.reverse else forest.head.indices
    var max = 0
    var lastIdx = 0
    val direction = if (from == 0 || from == 3) 1 else -1
    if (from == 1 || from == 3) {
      rowsToIterate.foreach { i =>
        colsToIterate.foreach { j =>
          val v = forest(i)(j)
          if (j != colsToIterate.head) {
            val prevCanSee = scenicScores(i)(j - (1 * direction))
            val prevHeight = forest(i)(j - (1 * direction))
            if (v > max) {
              max = v
              scenicScores.update(i, scenicScores(i).updated(j, math.abs(colsToIterate.head - i)))
            } else {
              if (v > prevHeight)
                scenicScores.update(i, scenicScores(i).updated(j, prevCanSee + 1))
              else {
                scenicScores.update(i, scenicScores(i).updated(j, 1))
              }
            }
          }
        }
        max = 0
      }
    } else {
      colsToIterate.foreach { j =>
        rowsToIterate.foreach { i =>
          val v = forest(i)(j)
          if (i != rowsToIterate.head) {
            val prevCanSee = scenicScores(i - (1 * direction))(j)
            val prevHeight = forest(i - (1 * direction))(j)
            if (v > max) {
              max = v
              scenicScores.update(i, scenicScores(i).updated(j, math.abs(rowsToIterate.head - i)))
            } else {
              if (v > prevHeight) {
                scenicScores.update(i, scenicScores(i).updated(j, prevCanSee + 1))
              } else {
                scenicScores.update(i, scenicScores(i).updated(j, 1))
              }
            }
          }
        }
        max = 0

      }
    }
    scenicScores
  }

  def solve2: Int = {
    val scenicScoresR = forest.indices.map { i =>
      forest.head.indices.map { j => {
        val v = forest(i)(j)
        // to right
        var k = j + 1
        var canSeeRight = 0
        var cond = k < forest.head.length
        while (cond) {
          canSeeRight += 1
          if (forest(i)(k) >= v) cond = false
          else {k+=1; cond = k < forest.head.length}
        }
        // to left
        k = j - 1
        var canSeeLeft = 0
        cond = k >= 0
        while (cond) {
          canSeeLeft += 1
          if (forest(i)(k) >= v) cond = false
          else {k-=1; cond = k >= 0}
        }
        // down
        k = i + 1
        var canSeeDown = 0
        cond = k < forest.length
        while (cond) {
          canSeeDown += 1
          if (forest(k)(j) >= v) cond = false
          else {k+=1; cond = k < forest.length}
        }
        // up
        k = i - 1
        var canSeeUp = 0
        cond = k >= 0
        while (cond) {
          canSeeUp += 1
          if (forest(k)(j) >= v) cond = false
          else {k-=1; cond = k >= 0}
        }
        canSeeUp * canSeeDown * canSeeLeft * canSeeRight
      }}.toArray
    }.toArray
    scenicScoresR.map(a => a.max).max
  }
}
