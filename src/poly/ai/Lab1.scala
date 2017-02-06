package poly.ai

sealed abstract case class Bit(value: Int){
  def isSet: Boolean
  def unary_!(): Bit
  def |(other: Bit): Bit
  def \/(other: Bit): Bit
  def &(other: Bit): Bit
}


final case class One extends Bit(1) {
  override def isSet = true
  override def unary_!() = Zero()
  override def |(other: Bit) = One()
  override def \/(other: Bit) = other match {
    case One()  => Zero()
    case Zero() => One()
  }
  override def &(other: Bit): Bit = other match {
    case One() => One()
    case Zero() => Zero()
  }
}

final case class Zero extends Bit(0) {
  override def isSet = false
  override def unary_!() = One()
  override def |(other: Bit) = other
  override def \/(other: Bit) = other match {
    case One() => One()
    case Zero() => Zero()
  }
  override def &(other: Bit): Bit = Zero()
}

final case class Bitset(vals: Seq[Bit]){
  def isSet(index: Int): Boolean = vals(index).isSet
  def allSet(): Boolean = vals.forall(_.isSet)
  def !(): Bitset = Bitset(vals.map(b => !b))

  def +(other: Bitset): Bitset = {
    val (bigger, smaller) = biggerAndSmaller(this, other)
    val res = bigger.vals.zipWithIndex.map { case (bit, i) =>
      if(i <= smaller.vals.size - 1) bigger.vals(i) | smaller.vals (i)
      else bit
    }
    Bitset(res)
  }

  def *(other: Bitset): Bitset = {
    val (bigger, smaller) = biggerAndSmaller(this, other)
    val res = bigger.vals.zipWithIndex.map { case (bit, i) =>
      if(i <= smaller.vals.size - 1) bigger.vals(i) & smaller.vals(i)
      else Zero()
    }
    Bitset(res)
  }

  def -(other: Bitset): Bitset = {
    val (bigger, smaller) = biggerAndSmaller(this, other)
    val res = bigger.vals.zipWithIndex.map { case (bit, i) =>
      if(i <= smaller.vals.size - 1)
        bigger.vals(i) match {
          case Zero() => Zero()
          case One() => smaller.vals(i) match {
            case One()  => Zero()
            case Zero() => One()
          }
        }
      else bit
    }
    Bitset(res)
  }

  private def biggerAndSmaller(l: Bitset, r: Bitset): (Bitset, Bitset) =
    if(l.vals.size > r.vals.size) (l, r) else (r, l)
}

object Bitset {
  def empty(): Bitset = new Bitset(List.empty)
  def apply(vals: Seq[Int]): Bitset = new Bitset(vals.map(i =>
    if(i > 0) One() else Zero()))
}

sealed case class O[T](universal: U[T], bits: Bitset){
  override def toString: String = universal.decode(bits).toString()
}

object O {
  def empty(): O[Any] = O(U(List.empty), Bitset.empty())
}

final case class U[T] (ts: Seq[T]) {
  private val values = ts.distinct.toList

  def produce(elements: Seq[T]): O[T] = new O[T](this, produceBits(elements))

  def decode(bits: Bitset): List[T] =
    bits.vals.toList.zipWithIndex.collect { case (One(), i) => values(i) }

  private def produceBits(elems: Seq[T]): Bitset = {
    val dist = elems.distinct
    val bits = values.map(v => if(dist.contains(v)) 1 else 0)
    Bitset(bits)
  }
}

object Lab1 {
  def main(args: Array[String]): Unit = {
  }
}
