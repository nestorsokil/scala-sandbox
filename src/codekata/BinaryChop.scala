package codekata

object BinaryChop {
  def bSearchIter[A <: Comparable[A]](element: A, xs: List[A]) = {
    var right = xs.size - 1
    var left = 0
    while(right - left > 1){
      val middle = (left + right)/2
      if(element.compareTo(xs(middle)) < 0)
        right = middle
      else
        left = middle
    }
    if (xs(left).compareTo(element) == 0) left
    else -1
  }

  def bSearchRec[A <: Comparable[A]](element: A, xs: List[A]) = {
    def loopThru(lst: List[A]): Int = {
      val m = lst(lst.size / 2)
      if(m.compareTo(element) == 0) xs.indexOf(m)
      else {
        if (element.compareTo(m) < 0)
          loopThru(lst take lst.size / 2)
        else
          loopThru(lst drop lst.size / 2)
      }
    }
    loopThru(xs)
  }

  def searchMatch[A <: Comparable[A]](element: A, xs: List[A]): Int = {
    def loop(list: List[A]): Int = {
      list match {
        case Nil => -1
        case e :: Nil if e.compareTo(element) == 0 => xs.indexOf(e)
        case e :: _ if e.compareTo(element) == 0 => xs.indexOf(e)
        case e :: rest => loop(rest)
      }
    }
    loop(xs)
  }

  def measure[A <: Comparable[A]](f: (A, List[A]) => Int, a: A, as: List[A]): Long = {
    def now() = System.nanoTime()
    val ns = now()
    f(a, as)
    now() - ns
  }

  def main(args: Array[String]) {
    val test = List[Integer](1, 12, 44, 124, 222, 241, 251, 300)
    println(measure(bSearchIter[Integer], 124: Integer, test))
    println(measure(bSearchRec[Integer], 124: Integer, test))
    println(measure(searchMatch[Integer], 124: Integer, test))
  }

}
