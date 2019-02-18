package com.github.robfitzgerald.banditsearch.searchcontext

import cats.data.OptionT
import cats.implicits._
import cats.{Foldable, Monad, Monoid}

import com.github.robfitzgerald.banditsearch.banditnode.{BanditChild, SearchState}
import com.github.robfitzgerald.banditsearch.pedrosorei.Payload
import com.github.robfitzgerald.banditsearch.sampler.pedrosorei.{UCBPedrosoReiGlobalState, UCBPedrosoReiGlobals, UCBPedrosoReiSampler}
import spire.math.Numeric

object GenericPedrosoReiSynchronization {

  def synchronize[G[_] : Monad : Foldable, S, A, V: Numeric](
    payloads: G[Payload[S,A,V]],
    Sampler : UCBPedrosoReiSampler[S, A, V]
  )(implicit m: Monoid[UCBPedrosoReiGlobalState[S, A, V]]): G[Payload[S,A,V]] = {

    type FTransformer[B] = OptionT[G, B]

    val allActiveGlobals: FTransformer[UCBPedrosoReiGlobalState[S, A, V]] =
      for {
        payload <- OptionT(payloads.map { _._2 })
      } yield {
        payload.state
      }

    val bestGlobalState: UCBPedrosoReiGlobalState[S, A, V] = Foldable[FTransformer].fold(allActiveGlobals)

    payloads.map { payload =>
      val (parent, globalsOption) = payload
      if (parent.searchState != SearchState.Activated) payload
      else {
        val updatedGlobalsOption: Option[UCBPedrosoReiGlobals[S, A, V]] = globalsOption.map {
          _.copy(state = bestGlobalState)
        }
        val updatedParent =
          updatedGlobalsOption match {
            case None => parent
            case Some(updatedGlobals) =>
              val updatedChildren: Array[BanditChild[S, A, V]] =
                parent.children.map { child =>

                  val updatedReward = Sampler.rewardFunction(child.mctsStats, updatedGlobals, parent.mctsStats.observations)
                  child.copy(
                    reward = updatedReward
                  )
                }
              parent.copy(
                reward = Sampler.rewardFunction(parent.mctsStats, updatedGlobals, 0),
                children = updatedChildren
              )
          }

        (updatedParent, updatedGlobalsOption)
      }
    }
  }
}