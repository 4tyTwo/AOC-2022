package aoc

import scala.collection.mutable

case object Day7 extends AOCProblem[Int] {
  val day = 7

  private val input = scala.io.Source.fromFile("./priv/day7/input.txt").getLines().toSeq

  val lookup: mutable.Map[String, Dir] = mutable.Map.empty
  val commandsAndOutputs: Seq[Seq[String]] = input.mkString("\n").split('$').toSeq.map(_.trim.split("\n").map(_.trim).toSeq).tail

  sealed trait FSElem {
    def size: Int
    val name: String
  }

  case class Dir(name: String, parentPath: String, var contents: Seq[FSElem]) extends FSElem {
    def size: Int = contents.map(_.size).sum
    def down(to: String): Dir = {
      contents.collect { case d @ Dir(_, _, _) => d }.find(_.name == to).get
    }

    def updateContents(newContents:  Seq[FSElem]): Unit = contents = newContents
  }

  case class File(name: String, size: Int) extends FSElem

  def buildFS(commands: Seq[Seq[String]]) = {
    val root = Dir("/", "", Seq.empty)
    lookup.addOne("/", root)
    var currDir = root
    commands.foreach { commandAndResult =>
      val command = commandAndResult.head
      val result = commandAndResult.tail
      if (command.startsWith("cd")) {
        val whereTo = command.split(' ')(1)
        whereTo match {
          case "/" => currDir = root
          case ".." => currDir = lookup(currDir.parentPath)
          case other => currDir = currDir.down(other)
        }
      } else { // else it's ls
        val dirContents: Seq[FSElem] = result.map { line =>
          val parts = line.split(' ')
          if (parts(0) == "dir") {
            val newParentPath = if (currDir.name != "/") {
              if (currDir.parentPath != "/") {
                currDir.parentPath + "/" + currDir.name
              } else {
                "/" + currDir.name
              }
            } else "/"
            val newPath = if (newParentPath == "/") "/"  + parts(1) else newParentPath + "/" + parts(1)
            val newDir =  Dir(parts(1), newParentPath, Seq.empty)
            lookup.addOne(newPath, newDir)
            newDir
          } else {
            File(parts(1), parts(0).toInt)
          }
        }
        currDir.updateContents(dirContents)
        currDir
      }
    }
    root
  }

  val root = buildFS(commandsAndOutputs)

  def solve1: Int = {

    def countIfAbove(dir: Dir, threshold: Int): Int = {
      val dirSize = dir.size
      val c = dir.contents.collect { case d @ Dir(_, _, _) => d }.map(countIfAbove(_, threshold)).sum
      if (dirSize <= threshold) { dirSize + c } else c
    }

    countIfAbove(root, 100_000)
  }

  def solve2: Int = {
    def minSizeAbove(dir: Dir, threshold: Int): Option[Int] = {
      val dirSize = dir.size
      val c = dir.contents.collect { case d @ Dir(_, _, _) => d }.map(minSizeAbove(_, threshold)).filter(_.isDefined)
      if (c.nonEmpty) {
       c.min
      } else {
        if (dirSize >= threshold) Some(dirSize) else None
      }
    }

    val spaceUsed = root.size
    val spaceFree = 70_000_000 - spaceUsed
    val spaceNeededToFree = 30_000_000 - spaceFree
    minSizeAbove(root, spaceNeededToFree).get
  }
}
