package com.github.robfitzgerald.banditsearch.sampler.pedrosorei

import com.github.robfitzgerald.banditsearch.Objective
import spire.math.Numeric
import spire.implicits._

case class UCBPedrosoReiGlobalState [S, A, V : Numeric](gBest: V, gWorst: V, Cp: Double, bestSolution: Option[S] = None, bestAction: Option[A] = None) {
  def update(o: V, objective: Objective[V]): UCBPedrosoReiGlobalState[S,A,V] =
    if (objective.isMoreOptimalThan(o, gBest) && objective.isMoreOptimalThan(gWorst, o)) this.copy(gBest = o, gWorst = o)
    else if (objective.isMoreOptimalThan(o, gBest)) this.copy(gBest = o)
    else if (objective.isMoreOptimalThan(gWorst, o)) this.copy(gWorst = o)
    else this
}

object UCBPedrosoReiGlobalState {
  def apply[S, A, V : Numeric](
    objective: Objective[V],
    Cp: Double
  ): UCBPedrosoReiGlobalState[S,A,V] =
    new UCBPedrosoReiGlobalState[S,A,V](
      objective.badBounds,
      objective.optimalBounds,
      Cp
    )
}
