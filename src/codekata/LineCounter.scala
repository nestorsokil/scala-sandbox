package codekata

import java.io.File

import scala.annotation.tailrec
import scala.io.Source

object LineCounter {

  def countLines(dirs: File, extensions: Array[String], ignoreFolders: String*): Long = {
    val ext: (String => String) = fname => fname.substring(fname.lastIndexOf("."))

    val matchesExt: (String => Boolean) = filename => extensions.isEmpty || extensions.contains(ext(filename))

    val isIgnored: (File => Boolean) = dir => ignoreFolders.contains(dir.getName)

    @tailrec
    def loopFolders(dirs: Array[File], acc: Array[Long] = Array()): Array[Long] = {
      if(dirs.isEmpty) acc
      else {
        val inThisFolder = dirs.filter(f => f.isFile && matchesExt(f.getName)).map(countFileLines).sum
        val subfolders = dirs.filter(f => f.isDirectory && !isIgnored(f)).flatMap(dir => dir.listFiles())
        loopFolders(subfolders, acc :+ inThisFolder)
      }
    }

    def countFileLines(file: File): Long = {
      @tailrec
      def loopFile(lines: List[String], counter: Long, counting: Boolean): Long = {
        lines match {
          case Nil => counter
          case first :: rest if counting =>
            first.trim match {
              case s if s.startsWith("/*")  => loopFile (rest, counter, counting = false)
              case _                        => loopFile (rest, counter + 1, counting = true)
            }
          case first :: rest if !counting =>
            first.trim match {
              case s if s.startsWith("*/")  => loopFile (rest, counter, counting = true)
              case _                        => loopFile (rest, counter, counting = false)
            }
        }
      }

      val filtered =  Source.fromFile(file, "ISO-8859-1").getLines()
        .filterNot(line => line.isEmpty || line.trim.startsWith("//") || line.trim.startsWith("#"))
      loopFile(filtered.toList, 0, counting = true)
    }

    loopFolders(dirs.listFiles()).sum
  }

  def main(args: Array[String]): Unit = {
    val f = new File("C:\\Users\\nsokil\\IdeaProjects\\dpp8\\")
    val extensions = Array(".java", ".xml", ".py", ".sql", ".jsp")
    println(countLines(f, extensions, ".git", ".idea", "target"))
  }
}