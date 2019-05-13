package com.github.robfitzgerald.dabtree.spark.searchcontext

import cats.data.OptionT
import cats.implicits._
import cats.{Foldable, Monad, Monoid}

import com.github.robfitzgerald.dabtree.common.banditnode.SearchState
import com.github.robfitzgerald.dabtree.spark.banditnode.SparkBanditChild
import com.github.robfitzgerald.dabtree.spark.sampler.pedrosorei.Payload
import com.github.robfitzgerald.dabtree.spark.sampler.pedrosorei.{UCBPedrosoReiGlobalState, UCBPedrosoReiGlobals, UCBPedrosoReiSampler}

object DoublePrecisionSynchronization {

  def synchronize[G[_] : Monad : Foldable, S, A](
    payloads: G[Payload[S,A,Double]],
    Sampler : UCBPedrosoReiSampler[S, A]
  )(implicit m: Monoid[UCBPedrosoReiGlobalState[S, A, Double]]): G[Payload[S,A,Double]] = {

    type FTransformer[B] = OptionT[G, B]

    val allActiveGlobals: FTransformer[UCBPedrosoReiGlobalState[S, A, Double]] =
      for {
        payload <- OptionT(payloads.map { _._2 })
      } yield {
        payload.state
      }

    val bestGlobalState: UCBPedrosoReiGlobalState[S, A, Double] = Foldable[FTransformer].fold(allActiveGlobals)

    payloads.map { payload =>
      val (parent, globalsOption, stopTime, random) = payload
      if (parent.searchState == SearchState.Cancelled) payload
      else {
        val updatedGlobalsOption: Option[UCBPedrosoReiGlobals[S, A, Double]] = globalsOption.map {
          _.copy(state = bestGlobalState)
        }
        val updatedParent =
          updatedGlobalsOption match {
            case None => parent
            case Some(updatedGlobals) =>
              val updatedChildren: Array[SparkBanditChild[S, A]] =
                parent.children.map { child =>

                  val updatedReward = Sampler.rewardFunction(child.mctsStats, updatedGlobals, parent.mctsStats.observations.toInt)
                  child.copy(
                    reward = updatedReward
                  )
                }
              parent.copy(
                reward = Sampler.rewardFunction(parent.mctsStats, updatedGlobals, 0),
                children = updatedChildren
              )
          }

        (updatedParent, updatedGlobalsOption, stopTime, random)
      }
    }
  }
}