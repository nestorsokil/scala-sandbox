package poly.ai.lab1

/**
  * Created by nsokil on 07.02.2017.
  */

sealed case class O[T](universal: U[T], bits: Bitset) {
  def ∪(that: O[T]): O[T] = O(universal, this.bits OR that.bits)
  def ∩(that: O[T]): O[T] = O(universal, this.bits AND that.bits)
  def \(that: O[T]): O[T] = O(universal, this.bits - that.bits)

  override def toString: String = universal.decode(bits).mkString(", ")
}

object O {
  def empty(): O[Any] = O(U(Nil), Bitset.empty())
}