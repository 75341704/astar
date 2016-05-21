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
     */
    @tailrec
    def find(notYetExploded: Set[S], bestSoFar: S, alreadyExploded: Set[S]): S = {
      notYetExploded.toList.map(s => (s, optimisticHeuristic(s))).sortBy(-_._2).headOption match {
        case None =>
          bestSoFar
        case Some((_, heuristic)) if heuristic <= value(bestSoFar) =>
          bestSoFar
        case Some((toExplode, _)) =>
          val exploded = explode(toExplode) diff alreadyExploded
          val result = if (value(toExplode) > value(bestSoFar)) toExplode else bestSoFar
          find((notYetExploded.filter(_ != toExplode) ++ exploded).filter(optimisticHeuristic(_) > value(result)), result, alreadyExploded + toExplode)
      }
    }

    find(Set(initial), initial, Set())
  }


}
