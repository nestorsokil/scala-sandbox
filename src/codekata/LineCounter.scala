package codekata

import java.io.File
import scala.io.Source

object LineCounter {

  def iterFiles(dirs: File, ignoreFolders: String*): Long = {
    def countLines(file: File) =
      Source.fromFile(file, "ISO-8859-1").getLines()
        .filterNot(line => line.trim.startsWith("//") || line.isEmpty || line.trim.startsWith("#"))
        .size.toLong

    def matchesExt(filename: String, patterns: String*): Boolean = {
      if(patterns.isEmpty) true
      else {
        val actual = filename.substring(filename.lastIndexOf("."))
        patterns.contains(actual)
      }
    }

    def isIgnored(dir: File): Boolean = ignoreFolders.contains(dir.getName)

    dirs.listFiles().filter(f => f.isFile && matchesExt(f.getName, ".java", ".xml", ".py", ".sql", ".jsp")).map(countLines).sum +
      dirs.listFiles().filter(dir => dir.isDirectory && !isIgnored(dir)).map(f => iterFiles(f, ignoreFolders : _*)).sum
  }

  def main(args: Array[String]): Unit = {
    val f = new File("C:\\Users\\nsokil\\IdeaProjects\\dpp8\\")
    println(iterFiles(f, ".git", ".idea", "target"))
  }

}
