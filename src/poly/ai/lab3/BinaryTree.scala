package poly.ai.lab3

/**
  * Created by nsokil on 02.03.2017.
  */
abstract class BinaryTree {
  def parent: BinaryTree
  def left: BinaryTree
  def right: BinaryTree
  def addLeft(a: Node): BinaryTree
  def addRight(a: Node): BinaryTree
  def value: Node

  private def checkAndTraverse(traversal: (BinaryTree => String)): String = {
    this match {
      case EmptyBTree() => ""
      case _ => traversal(this)
    }
  }

  def preOrder(): String =
    checkAndTraverse(
      { case SomeBTree(v, _, l, r) => Utils.concat(v.toString, l.preOrder(), r.preOrder()) })

  def inOrder(): String =
    checkAndTraverse(
      { case SomeBTree(v, _, l, r) => Utils.concat(l.inOrder(), v.toString, r.inOrder()) })

  def postOrder(): String =
    checkAndTraverse(
      { case SomeBTree(v, _, l, r) => Utils.concat(l.postOrder(), r.postOrder(), v.toString) })

  def result(acc: Operand): Operand = {
    this match {
      case EmptyBTree() => acc
      case SomeBTree(v, p, l, r) =>
        this.value match {
          case op    @ Operand() => op
          case bifun @ Binary()  => bifun.perform(l.result(acc), r.result(acc))
          case ufun  @ Unary()   => ufun.perform(l.result(acc))
        }
    }
  }
}

case class EmptyBTree() extends BinaryTree {
  override def left: BinaryTree = ???
  override def right: BinaryTree = ???
  override def parent: BinaryTree = ???
  override def value: Node = ???
  override def addLeft(a: Node): BinaryTree  = SomeBTree(a, this, EmptyBTree(), EmptyBTree())
  override def addRight(a: Node): BinaryTree = SomeBTree(a, this, EmptyBTree(), EmptyBTree())
}

case class SomeBTree(value: Node, parent: BinaryTree,
                        var left: BinaryTree, var right: BinaryTree) extends BinaryTree{
  override def addLeft(a: Node): BinaryTree  = {
    left = SomeBTree(a, this, EmptyBTree(), EmptyBTree()); left
  }
  override def addRight(a: Node): BinaryTree = {
    right = SomeBTree(a, this, EmptyBTree(), EmptyBTree()); right
  }
}

