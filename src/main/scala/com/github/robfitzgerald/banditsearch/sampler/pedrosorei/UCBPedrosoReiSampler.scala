package com.github.robfitzgerald.banditsearch.sampler.pedrosorei

import cats.Monad

import com.github.robfitzgerald.banditsearch.Objective
import com.github.robfitzgerald.banditsearch.banditnode._
import com.github.robfitzgerald.banditsearch.mctsstats.mutable.MCTSStatsMutableImpl
import com.github.robfitzgerald.banditsearch.randomselection.RandomSelection
import com.github.robfitzgerald.banditsearch.reward.UCBPedrosoRei
import com.github.robfitzgerald.banditsearch.sampler.{CanSample, Sampler}
import com.github.robfitzgerald.banditsearch.mctsstats.implicits._
import spire.algebra._
import spire.implicits._
import spire.math._

case class UCBPedrosoReiSampler[S, A, V : Numeric : Trig](
  parent                      : BanditParent[S,A,V],
  samplerState                : UCBPedrosoReiSamplerState[S, A, V],
  override val simulate       : S => S,
  override val evaluate       : S => V,
  override val objective      : Objective[V],
  override val randomSelection: BanditParent[S, A, V] => Int = RandomSelection.complimentaryMultiplyWithCarry[S, A, V]
) extends CanSample[S, A, V, UCBPedrosoReiSamplerState[S, A, V]] {

  final def updateStats: (MCTSStatsMutableImpl[V], V) => Unit =
    (stats, observation) => stats.update(observation)

  final def updateSamplerState: (UCBPedrosoReiSamplerState[S, A, V], V) => Unit =
    (currentSamplerState, observation) => currentSamplerState.update(observation, objective)

  final def rewardFunction: (MCTSStatsMutableImpl[V], UCBPedrosoReiSamplerState[S, A, V], Int) => Reward = {
    (stats, state, pVisits) =>
      UCBPedrosoRei.rewardFunction[V](
        state.gBest,
        state.gWorst,
        objective.optimal(stats.min, stats.max),
        stats.mean,
        pVisits,
        stats.observations,
        state.Cp
      )
  }
}

object UCBPedrosoReiSampler {
  def initial[F[_] : Monad, S, A, V: Numeric : Trig](
    parent    : BanditParent[S, A, V], // todo: this initial shouldn't take this arg, should build parent
    simulate  : S => S,
    evaluate  : S => V,
    objective : Objective[V],
    Cp        : Double = math.sqrt(2)
  ): UCBPedrosoReiSampler[S, A, V] = {
    val ucbPedrosoReiSamplerState = UCBPedrosoReiSamplerState[S, A, V](objective.badBounds, objective.optimalBounds, Cp)
    new UCBPedrosoReiSampler[S, A, V](parent, ucbPedrosoReiSamplerState, simulate, evaluate, objective)
  }

  def promote[F[_], S, A, V: Numeric](
  ): UCBPedrosoReiSampler[S, A, V] = {
    ???
  }
}