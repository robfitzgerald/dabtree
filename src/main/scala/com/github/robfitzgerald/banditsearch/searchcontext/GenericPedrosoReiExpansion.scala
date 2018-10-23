package com.github.robfitzgerald.banditsearch.searchcontext

import cats.Monad
import cats.implicits._

import com.github.robfitzgerald.banditsearch.banditnode.{BanditChild, BanditParent}
import com.github.robfitzgerald.banditsearch.sampler.pedrosorei.UCBPedrosoReiGlobals
import spire.math.Numeric

object GenericPedrosoReiExpansion {

  def expand[G[_] : Monad, S, A, V: Numeric](
    observationsThreshold: Int,
    rewardThreshold: Double,
    maxExpandPerIteration: Int,
    stopExpandFunction: S => Boolean,
    evaluate: Option[S => V],
    generateChildren: S => Array[(S, Option[A])]
  )(payload: (BanditParent[S, A, V], Option[UCBPedrosoReiGlobals[S, A, V]])): G[(BanditParent[S, A, V], Option[UCBPedrosoReiGlobals[S, A, V]])] = {
    if (payload._1.mctsStats.observations < observationsThreshold) Monad[G].pure(payload)
    else if (stopExpandFunction(payload._1.state)) Monad[G].pure(payload)
    else {
      payload._2 match {
        case None => Monad[G].pure(payload)
        case Some(globals) =>
          val expandedChildren: Array[(BanditParent[S,A,V], Int)] =
            payload._1.
              children.
              zipWithIndex.
              filter {
                _._1.reward >= rewardThreshold
              }.
              sortBy {
                -_._1.reward
              }.
              take(maxExpandPerIteration).
              map { case (child: BanditChild[S, A, V], index: Int) =>
                (BanditChild.promote(child, evaluate, generateChildren), index)
              }

          // todo: remove expanded children from parent
          val updatedChildren = payload._1.children
          val updatedParent = payload._1

          // todo: lift expandedChildren to G, add to parent, return
          ???
      }
    }
  }

}
