package com.github.robfitzgerald.dabtree.spark.searchcontext

import org.apache.spark.rdd.RDD

import com.github.robfitzgerald.dabtree.common.SearchStats
import com.github.robfitzgerald.dabtree.spark.mctsstats.immutable.MCTSStatsImmutableDoublePrecisionImpl
import com.github.robfitzgerald.dabtree.spark.sampler.pedrosorei.Payload
import com.github.robfitzgerald.dabtree.spark.objective.Objective
import com.github.robfitzgerald.dabtree.spark.searchcontext.CancelledPayloadAccumulator.CancelledData

object DoublePrecisionCollect {
  case class CollectResult[S](
    bestState: Option[S] = None,
    bestCost: Option[Double] = None,
    payloadStateTransitions: Int = 0,
    payloadsCount: Int = 0,
    activatedCount: Int = 0,
    suspendedCount: Int = 0,
    cancelledCount: Int = 0,
    samples: Long = 0,
    iterations: Int = 0
  ) {

    def addIterations(iterations: Int): CollectResult[S] =
      this.copy(iterations = iterations)

    def + (stats: SearchStats, samples: Long): CollectResult[S] = {
      this.copy(
        payloadStateTransitions = this.payloadStateTransitions + stats.totalStateTransitions,
        payloadsCount = this.payloadsCount + 1,
        activatedCount = this.activatedCount + stats.activated,
        suspendedCount = this.suspendedCount + stats.suspended,
        cancelledCount = this.cancelledCount + stats.cancelled,
        samples = this.samples + samples
      )
    }

    def + (that: CollectResult[S]): CollectResult[S] = {
      val (newBestState, newBestCost) = this.bestCost match {
        case None => (that.bestState, that.bestCost)
        case Some(thisCost) =>
          that.bestCost match {
            case None => (this.bestState, this.bestCost)
            case Some(thatCost) =>
              if (thisCost < thatCost) (this.bestState, this.bestCost)
              else (that.bestState, that.bestCost)
          }
      }

      this.copy(
        bestState = newBestState,
        bestCost = newBestCost,
        payloadStateTransitions = this.payloadStateTransitions + that.payloadStateTransitions,
        payloadsCount = this.payloadsCount + that.payloadsCount,
        activatedCount = this.activatedCount + that.activatedCount,
        suspendedCount = this.suspendedCount + that.suspendedCount,
        cancelledCount = this.cancelledCount + that.cancelledCount,
        samples = this.samples + that.samples
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


  def collectDoublePrecision[S, A](payloads: RDD[Payload[S, A, Double]], cancelledData: CancelledData, objective: Objective[Double], iterations: Int): Option[CollectResult[S]] = {
    if (payloads.isEmpty) None
    else {
      val initialAccumulatorState = CollectResult[S]().addIterations(iterations) + (cancelledData.searchStats, cancelledData.observations)
      val collectResult = payloads.
        aggregate(initialAccumulatorState)(
          (acc: CollectResult[S], payload: Payload[S, A, Double]) => {
            val (parent, globalsOption, _) = payload

            // adds search stats and samples to this accumulator
            val accStatsUpdate: CollectResult[S] = acc + (parent.searchStats, parent.mctsStats.observations)

            // get the accumulator's best cost and this payload's best cost
            val accumulatorCost: Double =
              acc.bestCost match {
                case None => objective.badBounds
                case Some(bestCost) => bestCost
              }
            val mctsStats: MCTSStatsImmutableDoublePrecisionImpl = parent.mctsStats
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
          },
          (a, b) => a + b
        )
      Some(collectResult)
    }
  }
}
