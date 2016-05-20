import Pricing._

object Data {
  val basket = Vector(
    Item("Zwitsal", "Huidolie", price = 10),
    Item("Zwitsal", "Bodylotion", price = 8),
    Item("Zwitsal", "Shampoo", price = 9),
    Item("Andrelon", "Shampoo", price = 6),
    Item("Nivea", "Shampoo", price = 7)
  )

  val promotions = Set(
    Promotion(4, Some(7), None, None),
    Promotion(3, None, Some("Zwitsal"), None),
    Promotion(3, None, None, Some("Shampoo"))
  )
}

object Main extends App {
  import Data._
  val pricing = new Pricing(basket, promotions)
  val solution = pricing.solution
  println(s"Result with discount ${pricing.discountValue(solution)}:")
  println(solution.mkString("\n"))
}
