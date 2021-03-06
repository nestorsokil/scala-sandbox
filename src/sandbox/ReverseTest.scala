package sandbox
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try

object ReverseTest {
  def reverse[T](list: List[T]): List[T] = list.reverse


  def reversePat[T](list: List[T]): List[T] = {
    list match {
      case Nil => Nil
      case head :: tail => reversePat(tail) :+ head
    }
  }

  def reverseTail[T](list: List[T]): List[T] = {
    @tailrec
    def loop(subj: List[T], acc: List[T]): List[T] =
      subj match {
        case Nil => acc
        case head :: tail => loop(tail, head :: acc)
      }
    loop(list, Nil)
  }

  def reverseFold[T](xs: List[T]): List[T] = {
    xs.foldLeft(List[T]())((x, y) => y :: x)
  }

  def reverseMutable[T](xs: List[T]): List[T] = {
    val mxs = mutable.MutableList(xs).flatten
    for(i <- 0 until xs.size / 2){
      val t = mxs(i)
      mxs(i) = mxs(mxs.size - i - 1)
      mxs(mxs.size - i - 1) = t
    }
    mxs.toList
  }

  def now() = System.nanoTime()

  def main(args: Array[String]) {
    Try
    val ints = 1 to 1000 toList
    val t1 = now(); reverse(ints); println(now() - t1)
    val t2 = now(); reversePat(ints); println(now() - t2)
    val t3 = now(); reverseFold(ints); println(now() - t3)
    val t4 = now(); reverseMutable(ints); println(now() - t4)
    val t5 = now(); reverseTail(ints); println(now() - t5)
    println(reversePat(ints))
    println(reverseTail(ints))
  }
}
