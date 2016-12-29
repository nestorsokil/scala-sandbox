package codekata

import java.io.File

import scala.annotation.tailrec
import scala.io.Source

object LineCounter {

  def countLines(dirs: File, extensions: Array[String], ignoreFolders: String*): Long = {
    @tailrec
    def loopFile(lines: List[String], accumulator: Long, counting: Boolean): Long = {
      lines match {
        case Nil => accumulator
        case head :: rest =>
          if (counting)
            if (head.trim.startsWith ("/*") )
              loopFile (rest, accumulator, counting = false)
            else
              loopFile (rest, accumulator + 1, counting = true)
          else if (head.trim.startsWith ("*/") )
            loopFile (rest, accumulator, counting = true)
          else loopFile (rest, accumulator, counting = false)
      }
    }

    def countFileLines(file: File) = {
      val filtered =  Source.fromFile(file, "ISO-8859-1").getLines()
        .filterNot(line => line.isEmpty || line.trim.startsWith("//") || line.trim.startsWith("#"))
      loopFile(filtered.toList, 0, counting = true)
    }


    def matchesExt(filename: String): Boolean = {
      if(extensions.isEmpty) true
      else {
        val actual = filename.substring(filename.lastIndexOf("."))
        extensions.contains(actual)
      }
    }

    def isIgnored(dir: File): Boolean = ignoreFolders.contains(dir.getName)

    @tailrec
    def loopFolders(acc: Array[Long], dirs: Array[File]): Array[Long] = {
      if(dirs.isEmpty) acc
      else {
        val inThisFolder = dirs.filter(f => f.isFile && matchesExt(f.getName)).map(countFileLines).sum
        loopFolders(acc :+ inThisFolder, dirs filter(f => f.isDirectory && !isIgnored(f)) flatMap(dir => dir.listFiles()))
      }
    }

    loopFolders(Array(), dirs.listFiles()).sum
  }

  def now() = System.nanoTime()

  def main(args: Array[String]): Unit = {
    val f = new File("C:\\Users\\nsokil\\IdeaProjects\\dpp8\\")
    println(countLines(f, Array(".java", ".xml", ".py", ".sql", ".jsp"), ".git", ".idea", "target"))
  }
}