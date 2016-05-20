import org.scalatest._

import Pricing._

class PricingSpec extends WordSpec with Matchers {
  "Pricing" should {
    val pricing = new Pricing(Data.basket, Data.promotions)

    "calculate an optimistic heuristic value for the empty state" in {
      pricing.optimisticHeuristic(State.empty) should be >(0)
    }

    "correctly apply promotions to baskets" in {
      val discountValue = 7
      val states = pricing.applyTo(State.empty, Promotion(4, Some(discountValue), None, None))
      states should have size(5)
      states.foreach(pricing.discountValue(_) should be (discountValue))
    }

    "correctly explode the empty state" in {
      pricing.explode(State.empty).size should be >(0)
    }

    "correctly calculate the right promotions to apply" in {
      val solution = pricing.solution
      solution.size should be >(0)
    }
  }
}
