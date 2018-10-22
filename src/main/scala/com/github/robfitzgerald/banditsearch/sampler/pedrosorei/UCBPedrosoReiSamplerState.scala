package com.github.robfitzgerald.banditsearch.sampler.pedrosorei

import com.github.robfitzgerald.banditsearch.Objective
import spire.math.Numeric
import spire.implicits._

case class UCBPedrosoReiSamplerState [S, A, V : Numeric](gBest: V, gWorst: V, Cp: Double, bestSolution: Option[S] = None, bestAction: Option[A] = None) {
  def update(o: V, objective: Objective[V]): UCBPedrosoReiSamplerState[S,A,V] =
    if (objective.isBetterThan(o, gBest) && objective.isBetterThan(gWorst, o)) this.copy(gBest = o, gWorst = o)
    else if (objective.isBetterThan(o, gBest)) this.copy(gBest = o)
    else if (objective.isBetterThan(gWorst, o)) this.copy(gWorst = o)
    else this
}
