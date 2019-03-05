
package com.github.robfitzgerald.dabtree.spark.sampler

import org.apache.spark.broadcast.Broadcast
import cats.Eval

import com.github.robfitzgerald.dabtree.common.DabTreeFunctionParameters
import com.github.robfitzgerald.dabtree.spark.banditnode.{SparkBanditChild, SparkBanditParent}
import com.github.robfitzgerald.dabtree.spark.mctsstats.HasStats
import com.github.robfitzgerald.dabtree.spark.mctsstats.mutable.MCTSStatsMutableDoublePrecisionImpl
import com.github.robfitzgerald.dabtree.spark.objective.Objective
import com.github.robfitzgerald.dabtree.spark.reward.UCBPedrosoRei
import com.github.robfitzgerald.dabtree.spark.sampler.pedrosorei.Payload
import com.github.robfitzgerald.dabtree.spark.sampler.pedrosorei.UCBPedrosoReiGlobals

class SamplerDoublePrecision[S, A] (
  simulate       : S => S,
  evaluate       : S => Double,
//  banditSelection: SparkBanditParent[S,A] => Int,
  objective      : Objective[Double]
) extends Serializable {

  def run(
    payload           : Payload[S, A, Double],
    samples           : Int
  ): Eval[Payload[S, A, Double]] = {

    val (parent: SparkBanditParent[S, A], globalsOption: Option[UCBPedrosoReiGlobals[S, A, Double]], stopTime: Long) = payload
    globalsOption match {
      case None =>
        Eval.now{payload}

      case Some(globals) =>
        Eval.later {
          // convert our data structures from immutable to mutable structures
          var updatedSamplerState: UCBPedrosoReiGlobals[S,A,Double] = globals
          var parentReward: Double = parent.reward
          val parentStats: MCTSStatsMutableDoublePrecisionImpl = parent.mctsStats.toMutable
          val childrenRewards: collection.mutable.Buffer[Double] = parent.children.map{ _.reward }.toBuffer
          val childrenStats: Array[MCTSStatsMutableDoublePrecisionImpl] = parent.children.map { _.mctsStats.toMutable }
          val childrenActions: IndexedSeq[A] = parent.children.flatMap { _.action }
          val childrenStates: IndexedSeq[S] = parent.children.map { _.state }

          // perform samples, keep track of the best-valued index
          var observations = 0
          var bestIdx = 0
          var bestReward = 0.0
          do {
            // basic MCTS sampling
            val selectedAction: A = childrenActions(bestIdx)
            val selectedState: S = childrenStates(bestIdx)
            val simulatedState: S = simulate(selectedState)
            val cost: Double = evaluate(simulatedState)

            // perform update on child and parent (globals via immutable semantics, parent/child via mutable)
            val selectedChildStats = childrenStats(bestIdx)
            updatedSamplerState = updateSamplerState(updatedSamplerState, simulatedState, selectedAction, cost)
            updateStats(selectedChildStats, cost)
            updateStats(parentStats, cost)

            // update all rewards, and note the best child along the way, for the next sample
            bestReward = 0.0
            for {
              childIdx <- childrenRewards.indices
            } {
              val updateChild = childrenStats(childIdx)
              val childRewardUpdate: Double = rewardFunction(updateChild, updatedSamplerState, parentStats.observations.toInt)
              if (bestReward < childRewardUpdate) {
                bestReward = childRewardUpdate
                bestIdx = childIdx
              }
              childrenRewards.update(childIdx, childRewardUpdate)
            }
            parentReward = rewardFunction(parentStats, updatedSamplerState, 0)

            observations += 1
          } while (observations < samples)

          // back to immutable structures. update all children with sampling results
          val updatedChildren: Array[SparkBanditChild[S, A]] =
            parent.children.
              zip(childrenRewards.zip(childrenStats)).
              map { case (child, (updatedReward, stats)) =>
                child.copy[S, A](
                  reward = updatedReward,
                  mctsStats = stats.toImmutable
                )
              }

          // update parent with updated children and sampling results
          val updatedParent = parent.copy[S, A](
            reward = parentReward,
            mctsStats = parentStats.toImmutable,
            children = updatedChildren
          )

          (updatedParent, Some(updatedSamplerState), stopTime)
        }

    }
  }

  final def updateStats: (MCTSStatsMutableDoublePrecisionImpl, Double) => Unit = (stats, observation) => {
    // update performed in-place on mutable data
    val _ = stats.update(observation)
  }

  final def updateSamplerState: (UCBPedrosoReiGlobals[S, A, Double], S, A, Double) => UCBPedrosoReiGlobals[S, A, Double] = (currentGlobals, simulatedState, chosenAction, observation) => {
    val updatedGlobalState = currentGlobals.state.update(simulatedState, chosenAction, observation, objective)
    currentGlobals.copy(state = updatedGlobalState)
  }


  final def rewardFunction(stats: HasStats, globals: UCBPedrosoReiGlobals[S, A, Double], pVisits: Int): Double = {
    UCBPedrosoRei.rewardFunction(
      globals.state.gBest,
      globals.state.gWorst,
      objective.optimal(stats.min, stats.max),
      stats.mean,
      pVisits,
      stats.observations.toInt,
      globals.meta.Cp
    )
  }
}

object SamplerDoublePrecision {
  def apply[S, A](
    simulate       : S => S,
    evaluate       : S => Double,
//    banditSelection: SparkBanditParent[S,A] => Int,
    objective      : Objective[Double]
  ): SamplerDoublePrecision[S, A] = new SamplerDoublePrecision[S, A](
    simulate, evaluate, objective
  )


  def apply[S, A](
    bCastFns            : Broadcast[DabTreeFunctionParameters[S, A, Double]],
//    bCastBanditSelection: Broadcast[SparkBanditParent[S,A] => Int],
    bCastObjective      : Broadcast[Objective[Double]]
  ): SamplerDoublePrecision[S, A] = new SamplerDoublePrecision[S, A](
    bCastFns.value.simulate, bCastFns.value.evaluate, bCastObjective.value
  )
}
