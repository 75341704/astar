import scala.annotation._

object Astar {

  def findOptimalSolution[S](
    initial: S,
    explode: S => Set[S],
    optimisticHeuristic: S => Int,
    value: S => Int
  ) = {
    @tailrec
    def find(currentCandidates: Set[S], resultSoFar: S, alreadyExploded: Set[S]): S = {
      val sorted = currentCandidates.toList.map(s => (s, optimisticHeuristic(s))).sortBy(_._2).reverse

      if (sorted.isEmpty) resultSoFar
      else if (sorted.head._2 <= value(resultSoFar)) resultSoFar
      else {
        val toExplode = sorted.find { case (candidate, _) => !alreadyExploded.contains(candidate) }.map(_._1)
        toExplode match {
          case None => resultSoFar
          case Some(candidate) =>
            val exploded = explode(candidate) diff alreadyExploded
            val result = if (value(candidate) > value(resultSoFar)) candidate else resultSoFar
            find((currentCandidates.filter(_ != candidate) ++ exploded).filter(optimisticHeuristic(_) > value(result)), result, alreadyExploded + candidate)
        }
      }
    }

    find(Set(initial), initial, Set())
  }


}
