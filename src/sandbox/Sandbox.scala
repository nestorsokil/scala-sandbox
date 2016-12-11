package sandbox

object Sandbox {
  def sign(x: Int) = if (x > 0) 1 else if (x < 0) -1 else 0

  def countdown(n: Int) {
    for (i <- n to 0 by -1) println(i)
  }

  def power(x: Double, pow: Int): Double = {
    if (pow > 0) {
      if (pow % 2 != 0)
        x * power(x, pow - 1)
      else {
        val y = power(x, pow / 2)
        y * y
      }
    }
    else if (pow == 0) 1
    else 1 / power(x, pow * (-1))
  }

  def filterByUpperLim(coll: Iterable[Int], limit: Int) = coll filter (_ < limit)

  class BankAccount(initialBalance: Double) {
    protected var balance = initialBalance

    def deposit(amount: Double) = {
      balance += amount
      balance
    }

    def withdraw(amount: Double) = {
      balance -= amount
      balance
    }
  }

  class CheckingAccount(initialBalance: Double) extends BankAccount(initialBalance) {
    override def deposit(amount: Double) = {
      comission(); super.deposit(amount)
    }

    override def withdraw(amount: Double) = {
      comission(); super.withdraw(amount)
    }

    private def comission() = balance -= 1
  }

  def squareList(xs: List[Int]): List[Int] =
    xs match {
      case Nil => xs
      case y :: ys => y*y :: squareList(ys)
    }

  def squareListM(xs: List[Int]): List[Int] =
    xs map (x => x*x)

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val (first, rest) = xs span(y => x == y)
      first :: pack(rest)
  }

  def encode[T](xs: List[T]): List[(T, Int)] = pack(xs).map(lst => (lst.head, lst.length))


  def isSorted[T](as: List[T], comparator: (T,T) => Boolean): Boolean = {
    as match {
      case Nil => true
      case _ :: Nil => true
      case x :: rest => comparator(x, rest.head) && isSorted(rest, comparator)
    }
  }

  def isArraySorted[T](xs: Array[T], comparator:(T,T) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if(n == 1) true
      else if(comparator(xs(n-1), xs(n))) loop(n-1)
      else false
    }
    loop(xs.length - 1)
  }


  def fib(n: Int): Int = {
    if(n == 0) 0
    else if(n == 1) 1
    else fib(n-1) + fib(n-2)
  }

  def not[A](f: A => Boolean): A => Boolean = !f(_)

  def main(args: Array[String]): Unit = {
    val f: Int => Boolean = _ < 5
    println(List(1, 64, 11, 2, 15, 5, 22) filter not(f))
  }

}
