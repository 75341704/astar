import org.scalatest._

class AstarSpec extends WordSpec with Matchers {
  "astar" should {
    "return the initial state when there's no exploded states" in {
      Astar.findOptimalSolution[Int](
        initial = 42,
        explode = _ => Set(),
        optimisticHeuristic = identity,
        value = identity
      ) should be(42)
    }

    "return the exploded state when they're higher-value" in {
      Astar.findOptimalSolution[Int](
        initial = 42,
        explode = _ => Set(43),
        optimisticHeuristic = _ => 43,
        value = identity
      )
    }

    "return the initial state when it is higher-value" in {
      Astar.findOptimalSolution[Int](
        initial = 42,
        explode = _ => Set(41),
        optimisticHeuristic = _ => 43,
        value = identity
      )
    }
  }
}
