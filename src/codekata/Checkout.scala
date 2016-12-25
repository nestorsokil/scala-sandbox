package codekata

import java.io.File

import scala.collection.mutable
import scala.collection.immutable
import scala.io.Source

case class Item(SKU: String, price: BigDecimal)

class Pricing()

case class SpecialPrice(amount: Int, price: BigDecimal) extends Pricing

case class NoSpecialPrice() extends Pricing

class Checkout(rulesFile: File) {
  private val (items, specials) = parseRules(rulesFile)

  private val cart = mutable.MutableList[Item]()

  private def parseRules(file: File): (immutable.Set[Item], Map[String, Pricing]) = {
    def toPair[T](array: Array[T]): (T, T) = (array(0), array(1))

    def parseItems(lines: List[String]) = lines
      .map(line => {
        val parts = line.split("-").map(_.trim)
        Item(parts(0), BigDecimal.exact(parts(1)))
      }).toSet

    def parsePrices(lines: List[String]) = lines.map(line => {
      val parts = line.split("-").map(_.trim)
      parts.length match {
        case 2 => parts(0) -> NoSpecialPrice()
        case 3 => parts(0) -> {
          val (amount, price) = toPair(parts(2).split(" for "))
          SpecialPrice(Integer.valueOf(amount), BigDecimal.exact(price))
        }
      }
    }).toMap

    val lines = Source.fromFile(file).getLines().toList
    (parseItems(lines), parsePrices(lines))
  }

  def availableItems() = items

  def scan(item: Item): BigDecimal = {
    cart += item
    total()
  }

  def total(): BigDecimal = {
    cart.distinct.map(item => {
      val amount = cart.count(_ == item)
      specials(item.SKU) match {
        case NoSpecialPrice() => item.price * amount
        case SpecialPrice(specialAmount, specialPrice) =>
          val discounted = amount / specialAmount
          val regular = amount - discounted*specialAmount
          specialPrice * discounted + item.price * regular
      }
    }).sum
  }
}

object Checkout {
  def main(args: Array[String]) {
    val config = new File(getClass.getResource("/resources/pricing.dat").getFile)
    val checkout = new Checkout(config)
    val items = checkout.availableItems().toList

    val itemOne = items(0)
    val itemTwo = items(1)
    val itemThree = items(2)
    val itemFour = items(3)

    println(checkout.scan(itemOne))
    println(checkout.scan(itemTwo))
    println(checkout.scan(itemThree))
    println(checkout.scan(itemTwo))
    println(checkout.scan(itemTwo))
    println(checkout.scan(itemOne))
    println(checkout.scan(itemOne))
  }
}
