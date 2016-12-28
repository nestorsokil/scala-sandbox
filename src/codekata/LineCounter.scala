package codekata

import java.io.File

import scala.annotation.tailrec
import scala.io.Source

object LineCounter {

  def countLines(dirs: File, ignoreFolders: String*): Long = {
    def resolveComments(lines: Array[String]): Long = {
      val iterLines = lines.filterNot(line => line.trim.startsWith("//") || line.isEmpty || line.trim.startsWith("#"))
      var count = 0
      var counting = true
      for(l <- iterLines){
        if(!counting && l.trim.startsWith("*/"))
          counting = true
        if(counting && l.trim.startsWith("/*"))
          counting = false
        if(counting)
          count += 1
      }
      count
    }

    def countFileLines(file: File) =
      resolveComments(Source.fromFile(file, "ISO-8859-1").getLines().toArray)


    def matchesExt(filename: String, patterns: String*): Boolean = {
      if(patterns.isEmpty) true
      else {
        val actual = filename.substring(filename.lastIndexOf("."))
        patterns.contains(actual)
      }
    }

    def isIgnored(dir: File): Boolean = ignoreFolders.contains(dir.getName)

    @tailrec
    def loop(acc: Array[Long], dirs: Array[File]): Array[Long] = {
      if(dirs.isEmpty) acc
      else {
        val inThisFolder = dirs.filter(f => f.isFile && matchesExt(f.getName, ".java", ".xml", ".py", ".sql", ".jsp")).map(countFileLines).sum
        loop(acc :+ inThisFolder,
          dirs filter(f => f.isDirectory && !isIgnored(f)) flatMap(dir => dir.listFiles()))
      }
    }

    loop(Array(), dirs.listFiles()).sum
    //dirs.listFiles().filter(f => f.isFile && matchesExt(f.getName, ".java", ".xml", ".py", ".sql", ".jsp")).map(countLines).sum +
    //  dirs.listFiles().filter(dir => dir.isDirectory && !isIgnored(dir)).map(f => iterFiles(f, ignoreFolders : _*)).sum
  }

  def main(args: Array[String]): Unit = {
    val f = new File("C:\\Users\\nsokil\\IdeaProjects\\dpp8\\")
    println(countLines(f, ".git", ".idea", "target"))
  }
}