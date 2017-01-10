package codekata

import java.io.File

import scala.annotation.tailrec
import scala.io.Source

object LineCounter {
  def countLines(dir: File, extensions: Array[String], ignoreFolders: String*): Long = {
    val ext:           String => String       = fname => fname.substring(fname.lastIndexOf("."))
    val matchesExt:    String => Boolean      = fname => extensions.isEmpty || extensions.contains(ext(fname))
    val isDirIgnored:  File   => Boolean      = dir   => ignoreFolders.contains(dir.getName)
    val readLines:     File   => List[String] = file  => Source.fromFile(file, "ISO-8859-1").getLines().toList
    val isLineIgnored: String => Boolean      = line  => line.isEmpty || line.trim.startsWith("//")

    @tailrec
    def loopFolders(dirs: Array[File], acc: Long = 0): Long = {
      if(dirs.isEmpty) acc 
      else {
        val inThisFolder = dirs.filter(f => f.isFile && matchesExt(f.getName))
          .map(file => loopFile(readLines(file).filterNot(isLineIgnored))).sum
        val subfolders = dirs.filter(f => f.isDirectory && !isDirIgnored(f))
          .flatMap(dir => dir.listFiles())
        loopFolders(subfolders, acc + inThisFolder)
      }
    }

    @tailrec
    def loopFile(lines: List[String], counter: Long = 0, counting: Boolean = true): Long = {
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

    loopFolders(dir.listFiles())
  }

  def main(args: Array[String]): Unit = {
    val f = new File("/home/nestor0603/IdeaProjects/printproject")
    val extensions = Array(".java", ".scala", ".xml", ".sql", ".jsp")
    println(countLines(f, extensions, ".git", ".idea", "target"))
  }
}