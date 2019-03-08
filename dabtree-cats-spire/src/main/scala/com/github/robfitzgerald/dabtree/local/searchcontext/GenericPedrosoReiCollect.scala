package com.github.robfitzgerald.dabtree.local.searchcontext

import cats.Foldable
import cats.implicits._

import spire.math.Numeric
import spire.syntax.numeric._
import com.github.robfitzgerald.dabtree.local.sampler.pedrosorei.Payload
import com.github.robfitzgerald.dabtree.common.SearchStats
import com.github.robfitzgerald.dabtree.local.Objective
import com.github.robfitzgerald.dabtree.local.mctsstats.immutable.MCTSStatsImmutableImpl

object GenericPedrosoReiCollect {
  case class CollectResult[S, V](
    bestState: Option[S] = None,
    bestCost: Option[V] = None,
    payloadStateTransitions: Int = 0,
    payloadsCount: Int = 0,
    activatedCount: Int = 0,
    suspendedCount: Int = 0,
    cancelledCount: Int = 0,
    samples: Int = 0,
    iterations: Int = 0
  ) {
    def add (stats: SearchStats, samples: Int): CollectResult[S, V] = {
      this.copy(
        payloadStateTransitions = this.payloadStateTransitions + stats.totalStateTransitions,
        payloadsCount = this.payloadsCount + 1,
        activatedCount = this.activatedCount + stats.activated,
        suspendedCount = this.suspendedCount + stats.suspended,
        cancelledCount = this.cancelledCount + stats.cancelled,
        samples = this.samples + samples
      )
    }

    override def toString: String = bestCost match {
      case None => s"nothing found... payloads: $payloadsCount act: $activatedCount sus: $suspendedCount can: $cancelledCount samples: $samples iterations: $iterations"
      case Some(cost) => s"bestCost: $cost payloads: $payloadsCount act: $activatedCount sus: $suspendedCount can: $cancelledCount samples: $samples iterations: $iterations"
    }

    def toCSVString: String = bestCost match {
      case None => s",$payloadsCount,$activatedCount,$suspendedCount,$cancelledCount,$samples,$iterations"
      case Some(cost) => s"$cost,$payloadsCount,$activatedCount,$suspendedCount,$cancelledCount,$samples,$iterations"
    }
  }

  def csvHeader: String = s"cost,payloadsCount,activatedCount,suspendedCount,cancelledCount,samples,iterations"

  def collect[G[_] : Foldable, S, A, V : Numeric](payloads: G[Payload[S, A, V]], objective: Objective[V], iterations: Int): Option[CollectResult[S, V]] = {
    if (payloads.isEmpty) None
    else {
      val collectResult = payloads.foldLeft(CollectResult[S, V](iterations = iterations)) { (acc, payload) =>
        val (parent, globalsOption) = payload

        // adds search stats and samples to this accumulator
        val accStatsUpdate: CollectResult[S, V] = acc.add(parent.searchStats, parent.mctsStats.observations)

        // get the accumulator's best cost and this payload's best cost
        val accumulatorCost: V =
          acc.bestCost match {
            case None => objective.badBounds
            case Some(bestCost) => bestCost
          }
        val mctsStats: MCTSStatsImmutableImpl[V] = parent.mctsStats
        val bestSimulation = objective.bestSimulation(mctsStats.min, mctsStats.max)

        if (objective.isMoreOptimalThan(accumulatorCost, bestSimulation)) accStatsUpdate
        else {

          // update the best solution and best cost only if this payload is more optimal than accumulator
          {
            for {
              globals <- globalsOption
              bestState = globals.state.bestSolution
              bestCost = globals.state.bestCost
            } yield (bestState, bestCost)
          } match {
            case None => accStatsUpdate
            case Some((bestS, bestV)) =>
              accStatsUpdate.copy(
                bestState = bestS,
                bestCost = bestV
              )
          }
        }
      }
      Some(collectResult)
    }
  }
}
