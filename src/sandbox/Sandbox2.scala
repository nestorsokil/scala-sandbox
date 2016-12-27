package sandbox

object Sandbox2 {
  def isPrime(x: Int): Boolean = {
    (2 until x).forall(y => x % y != 0)
  }

  def secondPrime(): Int = {
    (1000 to 10000 toList).filter(isPrime)(1)
  }

  def secondPrimeStream(): Int = {
    (1000 to 10000 toStream).filter(isPrime)(1)
  }

  def sieve(s: Stream[Int]): Stream[Int] =
    s.head #:: sieve(s.tail filter(_ % s.head != 0))

  def now() = System.nanoTime()

  def main(args: Array[String]): Unit = {
    val f: (Int, Double, String) => String = (x, y, z) => x+y+z
  }
}
