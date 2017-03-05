package poly.ai.lab3

/**
  * Created by Admin on 05.03.2017.
  */
abstract class Node {
}

abstract case class Operand() extends Node

class AlgebraicOperand(val value: Double) extends Operand {
  override def toString: String = value.toString
}

class LogicalOperand(val value: Boolean) extends Operand {
  override def toString: String = value.toString
}

abstract class Operation[T <: Operand]() extends Node

abstract case class Unary[T <: Operand]() extends Operation[T] {
  def perform(a: T): T
}
abstract case class Binary[T <: Operand]() extends Operation[T] {
  def perform(a: T, b: T): T
}

abstract class UnaryAlgebraicOperation extends Unary[AlgebraicOperand]

abstract class BinaryAlgebraicOperation extends Binary[AlgebraicOperand] {
  protected def fn: (Double, Double) => Double
  override def perform(a: AlgebraicOperand, b: AlgebraicOperand): AlgebraicOperand =
    new AlgebraicOperand(fn(a.value, b.value))
}
class Addition() extends BinaryAlgebraicOperation {
  override protected def fn: (Double, Double) => Double = _ + _
  override def toString: String = "+"
}

class Subtraction() extends BinaryAlgebraicOperation {
  override protected def fn: (Double, Double) => Double = _ - _
  override def toString: String = "-"
}

class Multiplication() extends BinaryAlgebraicOperation {
  override protected def fn: (Double, Double) => Double = _ * _
  override def toString: String = "*"
}

class Division() extends BinaryAlgebraicOperation {
  override protected def fn: (Double, Double) => Double = _ / _
  override def toString: String = "/"
}

abstract class UnaryLogicalOperation extends Unary[LogicalOperand]
abstract class BinaryLogicalOperation extends Binary[LogicalOperand]{
  protected def fn: (Boolean, Boolean) => Boolean
  override def perform(a: LogicalOperand, b: LogicalOperand): LogicalOperand =
    new LogicalOperand(fn(a.value, b.value))
}

class Negation() extends UnaryLogicalOperation {
  override def perform(a: LogicalOperand): LogicalOperand = new LogicalOperand(!a.value)
  override def toString: String = "!"
}

class Conjunction() extends BinaryLogicalOperation {
  override protected def fn: (Boolean, Boolean) => Boolean = _ && _
  override def toString: String = "^"
}

class Disjunction() extends BinaryLogicalOperation {
  override protected def fn: (Boolean, Boolean) => Boolean = _ || _
  override def toString: String = "V"
}