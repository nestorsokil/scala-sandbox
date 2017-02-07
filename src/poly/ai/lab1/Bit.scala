package poly.ai.lab1

/**
  * Created by nsokil on 07.02.2017.
  */

sealed abstract class Bit() {
  def isSet: Boolean
  def unary_!(): Bit
  def |(other: Bit): Bit
  def \/(other: Bit): Bit
  def &(other: Bit): Bit
}

final case class One() extends Bit() {
  override def isSet = true
  override def unary_!() = Zero()
  override def |(other: Bit) = One()
  override def \/(other: Bit) = other match {
    case One()  => Zero()
    case Zero() => One()
  }
  override def &(other: Bit): Bit = other match {
    case One()  => One()
    case Zero() => Zero()
  }
}

final case class Zero() extends Bit() {
  override def isSet = false
  override def unary_!() = One()
  override def |(other: Bit) = other
  override def \/(other: Bit) = other match {
    case One()  => One()
    case Zero() => Zero()
  }
  override def &(other: Bit): Bit = Zero()
}
