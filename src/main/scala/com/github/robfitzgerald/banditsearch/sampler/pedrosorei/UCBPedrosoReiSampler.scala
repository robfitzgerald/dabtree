package com.github.robfitzgerald.banditsearch.sampler.pedrosorei

import cats.Monad

import com.github.robfitzgerald.banditsearch.Objective
import com.github.robfitzgerald.banditsearch.banditnode._
import com.github.robfitzgerald.banditsearch.mctsstats.MCTSStats
import com.github.robfitzgerald.banditsearch.mctsstats.implicits._
import com.github.robfitzgerald.banditsearch.mctsstats.mutable.MCTSStatsMutableImpl
import com.github.robfitzgerald.banditsearch.randomselection.RandomSelection
import com.github.robfitzgerald.banditsearch.reward.UCBPedrosoRei
import com.github.robfitzgerald.banditsearch.sampler.CanSample
import spire.algebra._
import spire.math._

/**
  * the application logic for sampling a given sub-tree payload. see [[CanSample]] for a high-level overview.
  * @param simulate a function for producing terminal states from partial states
  * @param evaluate a function for cost evaluation of a state
  * @param objective in our search, whether we seek to minimize or maximize the costs
  * @param randomSelection a method used to select randomly when searching
  * @tparam S user-provided state type
  * @tparam A user-provided action type
  * @tparam V user-provided value type
  */
class UCBPedrosoReiSampler[S, A, V : Numeric : Trig](
  override val simulate       : S => S,
  override val evaluate       : S => V,
  override val objective      : Objective[V],
  override val randomSelection: BanditParent[S, A, V] => Int = RandomSelection.complimentaryMultiplyWithCarry[S, A, V]
) extends CanSample[S, A, V, UCBPedrosoReiGlobals[S, A, V]] {

  final def updateStats: (MCTSStatsMutableImpl[V], V) => Unit = (stats, observation) => stats.update(observation)

  final def updateSamplerState: (UCBPedrosoReiGlobals[S, A, V], V) => Unit = (currentGlobals, observation) => currentGlobals.copy(state = currentGlobals.state.update(observation, objective))

  // we want to have access to an implicit MCTSStats[StatsType[V], V]. see testMagic.sc.
  // reward function should be generic to mutable/immutable types
  // because rewardFunction itself does not mutate state.

  final def rewardFunction[StatsType[_]](stats: StatsType[V], globals: UCBPedrosoReiGlobals[S, A, V], pVisits: Int)(implicit evidence: MCTSStats[StatsType[V], V]): Reward = {
    UCBPedrosoRei.rewardFunction[V](
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

//  type Payload[S,A,V] = (BanditParent[S,A,V], Option[UCBPedrosoReiGlobals[S,A,V]])

  def apply[S, A, V: Numeric : Trig](
    simulate  : S => S,
    evaluate  : S => V,
    objective : Objective[V]
  ): UCBPedrosoReiSampler[S, A, V] = {
    new UCBPedrosoReiSampler[S, A, V](simulate, evaluate, objective)
  }
}