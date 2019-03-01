package com.github.robfitzgerald.dabtree.spark.sampler.pedrosorei

import cats.Monoid

import com.github.robfitzgerald.dabtree.spark.objective.Objective

case class UCBPedrosoReiGlobalState [S, A, V](gBest: V, gWorst: V, bestSolution: Option[S] = None, bestAction: Option[A] = None, bestCost: Option[V] = None) {

  def update(s: S, a: A, o: V, objective: Objective[V]): UCBPedrosoReiGlobalState[S,A,V] =
    if (objective.isMoreOptimalThan(o, gBest) && objective.isMoreOptimalThan(gWorst, o)) {
      this.copy(
        gBest = o,
        gWorst = o,
        bestSolution = Some(s),
        bestAction = Some(a),
        bestCost = Some(o)
      )
    }
    else if (objective.isMoreOptimalThan(o, gBest)) {
      this.copy(
        gBest = o,
        bestSolution = Some(s),
        bestAction = Some(a),
        bestCost = Some(o)
      )
    }
    else if (objective.isMoreOptimalThan(gWorst, o)) {
      this.copy(
        gWorst = o
      )
    }
    else this

  def combine (that: UCBPedrosoReiGlobalState[S,A,V], objective: Objective[V]): UCBPedrosoReiGlobalState[S,A,V] = {
    val overallGBest: V = if (objective.isMoreOptimalThan(this.gBest, that.gBest)) this.gBest else that.gBest
    val overallGWorst: V = if (objective.isMoreOptimalThan(this.gWorst, that.gWorst)) that.gWorst else this.gWorst
    val (overallBestSolution, overallBestAction, overallBestCost) = UCBPedrosoReiGlobalState.pickBestFrom(this, that, objective)
    UCBPedrosoReiGlobalState(overallGBest, overallGWorst, overallBestSolution, overallBestAction, overallBestCost)
  }
}

object UCBPedrosoReiGlobalState extends UCBPerosoReioGlobalStateOps {

  def apply[S, A, V](objective: Objective[V]): UCBPedrosoReiGlobalState[S,A,V] =
    new UCBPedrosoReiGlobalState[S,A,V](
      objective.badBounds,
      objective.optimalBounds
    )

  def pickBestFrom[S,A,V](a: UCBPedrosoReiGlobalState[S,A,V], b: UCBPedrosoReiGlobalState[S,A,V], objective: Objective[V]): (Option[S], Option[A], Option[V]) = {
    if (a.bestCost.isEmpty && b.bestCost.isEmpty) (None, None, None)
    else if (a.bestCost.isEmpty) (b.bestSolution, b.bestAction, b.bestCost)
    else if (b.bestCost.isEmpty) (a.bestSolution, a.bestAction, a.bestCost)
    else {
      val (aCost, bCost) = (a.bestCost.get, b.bestCost.get)
      if (objective.isMoreOptimalThan(aCost, bCost)) (a.bestSolution, a.bestAction, a.bestCost)
      else (b.bestSolution, b.bestAction, b.bestCost)
    }
  }
}


trait UCBPerosoReioGlobalStateOps {
  implicit def ucbPedrosoReiGlobalStateMonoidOps[S, A, V](implicit objective: Objective[V]): Monoid[UCBPedrosoReiGlobalState [S, A, V]] = new Monoid[UCBPedrosoReiGlobalState [S, A, V]] {

    def empty: UCBPedrosoReiGlobalState[S, A, V] = UCBPedrosoReiGlobalState[S, A, V](objective)

    def combine(x: UCBPedrosoReiGlobalState[S, A, V], y: UCBPedrosoReiGlobalState[S, A, V]): UCBPedrosoReiGlobalState[S, A, V] = x.combine(y, objective)
  }
}