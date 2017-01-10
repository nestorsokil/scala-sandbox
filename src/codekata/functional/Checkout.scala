package codekata.functional

import java.io.File

case class Item(SKU: String, price: BigDecimal)

trait Pricing
case class NoSpecialPrice() extends Pricing
case class SpecialPrice(amount: Int, price: BigDecimal) extends Pricing

case class Checkout(items: 	  Set[Item], 
			   		        specials: Map[String, Pricing],
			   		        cart: 	  List[String]) {
  def map(f: Set[Item] => Set[Item]): Checkout = new Checkout(f(items), specials, cart)
  def availableItems(): Set[Item] = items

  def total(): BigDecimal = cart.distinct.map(SKU => {
    val amount = cart.count(_ == SKU)
    val price  = items.find(_.SKU == SKU).map(_.price).get
    specials(SKU) match {
      case NoSpecialPrice() => price * amount
      case SpecialPrice(specAmount, specPrice) =>
        val discounted = amount / specAmount
        val regular    = amount - discounted * specAmount
        specPrice * discounted + price * regular
    }
  }).sum

  def addItem(SKU: String): Checkout = items.find(_.SKU == SKU) match {
    case None    => this
    case Some(_) => new Checkout(items, specials, cart :+ SKU)
  }

  def printTo(printer: String): Checkout = ???
}

object Checkout{
  private def parseRules(file: File): (Set[Item], Map[String, Pricing]) = ???

  def apply(rulesFile: File) = {
    val (items, specs) = parseRules(rulesFile)
    val cart = Nil
    new Checkout(items, specs, cart)
  }

  def main(args: Array[String]): Unit = {
    val config = new File(getClass.getResource("/resources/pricing.dat").getFile)
    val checkout = Checkout(config)
    val items = checkout.availableItems().map(_.SKU).toArray

    val itemOne = items(0)
    val itemTwo = items(1)
    val itemThree = items(2)
    val itemFour = items(3)

    checkout.addItem(itemOne).addItem(itemOne)
  }
}
