package com.github.robfitzgerald.dabtree.spark.sampler.pedrosorei

import com.github.robfitzgerald.dabtree.spark.banditnode._
import com.github.robfitzgerald.dabtree.spark.mctsstats.HasStats
import com.github.robfitzgerald.dabtree.spark.mctsstats.mutable.MCTSStatsMutableDoublePrecisionImpl
import com.github.robfitzgerald.dabtree.spark.objective.Objective
import com.github.robfitzgerald.dabtree.spark.randomselection.RandomSelection
import com.github.robfitzgerald.dabtree.spark.sampler.CanSample
import com.github.robfitzgerald.dabtree.spark.reward.UCBPedrosoRei

/**
  * the application logic for sampling a given sub-tree payload. see [[CanSample]] for a high-level overview.
  * @param simulate a function for producing terminal states from partial states
  * @param evaluate a function for cost evaluation of a state
  * @param objective in our search, whether we seek to minimize or maximize the costs
  * @param randomSelection a method used to select randomly when searching
  * @tparam S user-provided state type
  * @tparam A user-provided action type
  */
class UCBPedrosoReiSampler[S, A](
  override val simulate       : S => S,
  override val evaluate       : S => Double,
  override val objective      : Objective[Double],
  override val randomSelection: SparkBanditParent[S, A] => Int = RandomSelection.scalaRandom[S, A]
) extends CanSample[S, A, Double, UCBPedrosoReiGlobals[S, A, Double]] with Serializable {

  final def updateStats(stats: MCTSStatsMutableDoublePrecisionImpl, observation: Double): Unit = {
    // update performed in-place on mutable data
    val _ = stats.update(observation)
  }

  final def updateSamplerState: (UCBPedrosoReiGlobals[S, A, Double], S, A, Double) => UCBPedrosoReiGlobals[S, A, Double] = (currentGlobals, simulatedState, chosenAction, observation) => {
    val updatedGlobalState = currentGlobals.state.update(simulatedState, chosenAction, observation, objective)
    currentGlobals.copy(state = updatedGlobalState)
  }

  // we want to have access to an implicit MCTSStats[StatsType[V], V]. see testMagic.sc.
  // reward function should be generic to mutable/immutable types
  // because rewardFunction itself does not mutate state.

  override final def rewardFunction(stats: HasStats, globals: UCBPedrosoReiGlobals[S, A, Double], pVisits: Int): Double = {
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

object UCBPedrosoReiSampler {

  def apply[S, A](
    simulate  : S => S,
    evaluate  : S => Double,
    objective : Objective[Double],
    randomSelection: SparkBanditParent[S, A] => Int = RandomSelection.scalaRandom[S, A]
  ): UCBPedrosoReiSampler[S, A] = {
    new UCBPedrosoReiSampler[S, A](simulate, evaluate, objective, randomSelection)
  }
}