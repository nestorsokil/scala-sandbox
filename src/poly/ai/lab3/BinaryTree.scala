package poly.ai.lab3

/**
  * Created by nsokil on 02.03.2017.
  */
abstract class BinaryTree[A] {
  def isEmpty: Boolean
  def isRoot: Boolean
  def add(a: A): BinaryTree[A] = SomeBTree(a, this, EmptyBTree(), EmptyBTree())
  def value(): A

  private def checkAndTraverse(traversal: (BinaryTree[A] => String)): String = {
    this match {
      case EmptyBTree() => "."
      case _ => traversal(this)
    }
  }

  def preOrder(): String =
    checkAndTraverse({ case SomeBTree(v, _, l, r) =>  v.toString + l.preOrder + r.preOrder })

  def inOrder(): String =
    checkAndTraverse({ case SomeBTree(v, _, l, r) =>  l.inOrder + v.toString + r.inOrder })

  def postOrder(): String =
    checkAndTraverse({ case SomeBTree(v, _, l, r) =>  l.inOrder + r.inOrder + v.toString })
}

case class EmptyBTree[A]() extends BinaryTree[A] {
  override def isEmpty: Boolean = true
  override def isRoot: Boolean = false

  override def value(): A = throw new NoSuchElementException("EmptyBTree has no value")
}

case class SomeBTree[A](value: A, parent: BinaryTree[A],
                        left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]{
  override def isEmpty: Boolean = false

  override def isRoot: Boolean = parent match {
    case EmptyBTree() => true
    case _ => false
  }
}

