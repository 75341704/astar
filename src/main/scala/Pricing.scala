object Pricing {
  case class Item(brand: String, product: String, price: Int)
  case class Promotion(numberOfProducts: Int, discount: Option[Int], brand: Option[String], product: Option[String])

  type State = Map[Item, Promotion]
  object State {
    val empty: State = Map.empty
  }
}

import Pricing._
class Pricing(basket: Vector[Item], promotions: Set[Promotion]) {

  def discountValue(s: State, promotion: Promotion): Int = s.filter(_._2 == promotion).map(_._1.price).min
  def discountValue(s: State): Int = s.values.toVector.distinct.map(promotion => promotion.discount.getOrElse(discountValue(s, promotion))).sum
  def mostExpensiveProduct(s: State) = (basket diff s.keys.toSeq).map(_.price).max
  def potentialValue(s: State): Int = (promotions diff s.values.toSet).map(_.discount.getOrElse(mostExpensiveProduct(s))).sum

  def applyTo(s: State, p: Promotion): Set[State] = {
    require(basket != null)
    val candidates = basket diff s.keys.toSeq
    combinations(p.numberOfProducts, candidates.toList)
      .map(items => s ++ items.map(i => (i -> p)).toMap)
      .toSet
  }

  def flatMapSublists[A,B](ls: List[A])(f: (List[A]) => List[B]): List[B] =
    ls match {
      case Nil => Nil
      case sublist@(_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
    }

  def combinations[A](n: Int, ls: List[A]): List[List[A]] =
    if (n == 0) List(Nil)
    else flatMapSublists(ls) { sl =>
      combinations(n - 1, sl.tail) map {sl.head :: _}
    }

  def explode(state: State): Set[State] = (promotions diff state.values.toSet).flatMap(p => applyTo(state, p))
  def optimisticHeuristic(state: State): Int = discountValue(state) + potentialValue(state)

  def solution = Astar.findOptimalSolution[State](
    initial = Map.empty,
    explode = explode,
    optimisticHeuristic = optimisticHeuristic,
    value = (state: State) => discountValue(state)
  )


}
