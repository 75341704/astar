import scala.annotation._

object Astar {

  def findOptimalSolution[S](
    initial: S,
    explode: S => Set[S],
    optimisticHeuristic: S => Int,
    value: S => Int
  ) = {
    /**
     * Actually perform the search.
     *
     * Possible optimizations:
     *  * keep the currentCandidates sorted instead of sorting them every time (faster)
     *  * remember heuristics (faster but more memory)
     *  * remove nodes with a heuristic smaller than the value of the result so far from both the currentCandidates and the alreadyExploded (less memory and probably faster)
     *
     * @param currentCandidates not-yet-exploded states
     * @param resultSoFar highest-valued state exploded so far
     * @param alreadyExploded already-exploded states (to avoid loops)
     */
    @tailrec
    def find(currentCandidates: Set[S], resultSoFar: S, alreadyExploded: Set[S]): S = {
      val sorted = currentCandidates.toList.map(s => (s, optimisticHeuristic(s))).sortBy(_._2).reverse

      if (sorted.isEmpty) resultSoFar
      else if (sorted.head._2 <= value(resultSoFar)) resultSoFar
      else {
        val candidate = sorted.head._1
        val exploded = explode(candidate) diff alreadyExploded
        val result = if (value(candidate) > value(resultSoFar)) candidate else resultSoFar
        find((currentCandidates.filter(_ != candidate) ++ exploded).filter(optimisticHeuristic(_) > value(result)), result, alreadyExploded + candidate)
      }
    }

    find(Set(initial), initial, Set())
  }


}
