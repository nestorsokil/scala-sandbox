package poly.ai.lab1

/**
  * Created by nsokil on 07.02.2017.
  */

object Bitset {
  def empty(): Bitset = new Bitset(Nil)

  def apply(vals: Seq[Int]): Bitset = new Bitset(vals.map(i => if (i > 0) One() else Zero()))

  private type Combinator = (Bit, Bit) => Bit
  private def constructOp(left: Bitset, right: Bitset, combinator: Combinator, orElse: Option[Bit]): Bitset = {
    val (bigger, smaller) = if (left.vals.size > right.vals.size) (left, right) else (right, left)
    val res = bigger.vals.zipWithIndex.map { case (bit, i) =>
      if (i <= smaller.vals.size - 1) combinator(bit, smaller.vals(i))
      else orElse match {
        case None => bit
        case Some(value) => value
      }
    }
    new Bitset(res)
  }
}

final class Bitset(val vals: Seq[Bit]){
  def isSet(index: Int): Boolean = vals(index).isSet
  def allSet(): Boolean = vals.forall(_.isSet)
  def unary_!(): Bitset = new Bitset(vals.map(b => !b))

  def +(other: Bitset): Bitset = Bitset.constructOp(this, other, _ | _ , None)
  def *(other: Bitset): Bitset = Bitset.constructOp(this, other, _ & _ , Some(Zero()))
  def \/(other: Bitset): Bitset = Bitset.constructOp(this, other, _ \/ _, Some(One()))
  def -(other: Bitset): Bitset = {
    val combine: Bitset.Combinator = (left, right) => (left, right) match {
      case (Zero(), _) => Zero()
      case (One(), One()) => Zero()
      case (One(), Zero()) => One()
    }
    Bitset.constructOp(this, other, combine, None)
  }
  def NOT(other: Bitset): Bitset  = !this
  def OR(other: Bitset):  Bitset  = this + other
  def AND(other: Bitset): Bitset  = this * other
  def XOR(other: Bitset): Bitset  = this \/ other
  def NOR(other: Bitset): Bitset  = !(this + other)
  def NAND(other: Bitset): Bitset = !(this * other)
}
