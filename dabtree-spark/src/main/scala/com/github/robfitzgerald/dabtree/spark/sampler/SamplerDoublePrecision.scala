
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
  simulate          : S => S,
  evaluate          : S => Double,
  randomSelection   : SparkBanditParent[S,A] => Int,
  objective         : Objective[Double]
) extends Serializable {

  def run(
    payload           : Payload[S, A, Double],
    samples           : Int
  ): Eval[Payload[S, A, Double]] = {

    val (parent: SparkBanditParent[S, A], globalsOption: Option[UCBPedrosoReiGlobals[S, A, Double]]) = payload
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

          // perform samples
          var i = 0
          do {
            // basic MCTS sampling
            val selectedChildIndex: Int = randomSelection(parent)
            val selectedAction: A = childrenActions(selectedChildIndex)
            val selectedState: S = childrenStates(selectedChildIndex)
            val simulatedState: S = simulate(selectedState)
            val cost: Double = evaluate(simulatedState)

            // perform update on child and parent (globals via immutable semantics, parent/child via mutable)
            val selectedChildStats = childrenStats(selectedChildIndex)
            updatedSamplerState = updateSamplerState(updatedSamplerState, simulatedState, selectedAction, cost)
            updateStats(selectedChildStats, cost)
            updateStats(parentStats, cost)
            val childRewardUpdate = rewardFunction(selectedChildStats, updatedSamplerState, parentStats.observations)
            childrenRewards.update(selectedChildIndex, childRewardUpdate)
            parentReward = rewardFunction(parentStats, updatedSamplerState, 0)

            i += 1
          } while (i < samples)

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

          (updatedParent, Some(updatedSamplerState))
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

  // we want to have access to an implicit MCTSStats[StatsType[Double], Double]. see testMagic.sc.
  // reward function should be generic to mutable/immutable types
  // because rewardFunction itself does not mutate state.

  final def rewardFunction(stats: HasStats, globals: UCBPedrosoReiGlobals[S, A, Double], pVisits: Int): Double = {
    UCBPedrosoRei.rewardFunction(
      globals.state.gBest,
      globals.state.gWorst,
      objective.optimal(stats.min, stats.max),
      stats.mean,
      pVisits,
      stats.observations,
      globals.meta.Cp
    )
  }
}

object SamplerDoublePrecision {
  def apply[S, A](
    simulate          : S => S,
    evaluate          : S => Double,
    randomSelection   : SparkBanditParent[S,A] => Int,
    objective         : Objective[Double]
  ): SamplerDoublePrecision[S, A] = new SamplerDoublePrecision[S, A](
    simulate, evaluate, randomSelection, objective
  )


  def apply[S, A](
    bCastFns: Broadcast[DabTreeFunctionParameters[S, A, Double]],
    bCastRandomSelection: Broadcast[SparkBanditParent[S,A] => Int],
    bCastObjective: Broadcast[Objective[Double]]
  ): SamplerDoublePrecision[S, A] = new SamplerDoublePrecision[S, A](
    bCastFns.value.simulate, bCastFns.value.evaluate, bCastRandomSelection.value, bCastObjective.value
  )
}
