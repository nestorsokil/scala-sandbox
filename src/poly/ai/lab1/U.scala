package poly.ai.lab1

/**
  * Created by nsokil on 07.02.2017.
  */

final case class U[T] (ts: Seq[T]) {
  private val values = ts.distinct.toList

  def produce(elements: Seq[T]): O[T] = O(this, produceBits(elements))
  def <<(elements: Seq[T]): O[T] = produce(elements)

  def decode(bits: Bitset): List[T] =
    bits.vals.toList.zipWithIndex.collect { case (One(), i) => values(i) }
  def <<(bits: Bitset): List[T] = decode(bits)

  def o(): O[T] = O(this, produceBits(ts))

  private def produceBits(elems: Seq[T]): Bitset = {
    val dist = elems.distinct
    val bits = values.map(v => if(dist.contains(v)) 1 else 0)
    Bitset(bits)
  }
}