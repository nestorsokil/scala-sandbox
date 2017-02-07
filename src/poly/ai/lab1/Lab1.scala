package poly.ai.lab1

object Lab1 {
  def main(args: Array[String]): Unit = {
    val uni = U(List("a", "b", "c", "d"))
    val A = uni.produce(List("f", "c", "b"))
    println(A)
  }
}
