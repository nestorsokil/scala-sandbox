package poly.ai.lab3

/**
  * Created by Admin on 05.03.2017.
  */
object Lab3 {
  def main(args: Array[String]) {
    val tree =  EmptyBTree().addLeft(new Multiplication())
    tree.addLeft(new AlgebraicOperand(10))
      .parent.addRight(new Addition()).addLeft(new AlgebraicOperand(2))
      .parent.addRight(new AlgebraicOperand(6))

    println(tree.preOrder())
    println(tree.inOrder())
    println(tree.postOrder())

    println(tree.result(new AlgebraicOperand(0)))
  }
}
