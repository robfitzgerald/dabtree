package com.github.robfitzgerald.dabtree.spark.searchcontext

import org.apache.spark.broadcast.Broadcast
import org.apache.spark.rdd.RDD

import com.github.robfitzgerald.dabtree.common.banditnode.SearchState
import com.github.robfitzgerald.dabtree.spark.banditnode.SparkBanditChild
import com.github.robfitzgerald.dabtree.spark.objective.Objective
import com.github.robfitzgerald.dabtree.spark.sampler.SamplerDoublePrecision
import com.github.robfitzgerald.dabtree.spark.sampler.pedrosorei.{Payload, UCBPedrosoReiGlobalState, UCBPedrosoReiGlobals}

object SparkDoublePrecisionSynchronization {
  def synchronize[S, A](
    payloads      : RDD[Payload[S,A,Double]],
    bCastSampler  : Broadcast[SamplerDoublePrecision[S, A]],
    bCastObjective: Broadcast[Objective[Double]]
  ): RDD[Payload[S,A,Double]] = {

    // unpack globals
    val allActiveGlobals: RDD[Option[UCBPedrosoReiGlobalState[S, A, Double]]] =
      payloads.
        flatMap {
          case (parent, globalsOption, _) if parent.searchState != SearchState.Cancelled =>
            globalsOption match {
              case Some(globals) => Iterator(Some(globals.state))
              case None => Iterator.empty
            }
          case _ => Iterator.empty
        }

    // take best global state
    val bestGlobalStateOption: Option[UCBPedrosoReiGlobalState[S, A, Double]] =
      allActiveGlobals.
        fold(Option.empty[UCBPedrosoReiGlobalState[S, A, Double]]) {
          (aOpt, bOpt) => aOpt match {
            case None => bOpt
            case Some(a) =>
              bOpt match {
                case None => aOpt
                case Some(b) =>
                  Some { a.combine(b, bCastObjective.value) }
              }
          }
        }

    bestGlobalStateOption match {
      case None => payloads
      case Some(bestGlobalState) =>

        // update all payloads with the best global state
        payloads.map { payload =>
          val (parent, globalsOption, stopTime) = payload
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

                      val updatedReward = bCastSampler.value.rewardFunction(child.mctsStats, updatedGlobals, parent.mctsStats.observations.toInt)
                      child.copy(
                        reward = updatedReward
                      )
                    }
                  parent.copy(
                    reward = bCastSampler.value.rewardFunction(parent.mctsStats, updatedGlobals, 0),
                    children = updatedChildren
                  )
              }

            (updatedParent, updatedGlobalsOption, stopTime)
          }
        }
    }
  }
}
