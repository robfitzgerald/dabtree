package com.github.robfitzgerald.banditsearch.sampler.pedrosorei

import cats.Monad

import com.github.robfitzgerald.banditsearch.Objective
import com.github.robfitzgerald.banditsearch.banditnode._
import com.github.robfitzgerald.banditsearch.randomselection.RandomSelection
import com.github.robfitzgerald.banditsearch.reward.UCBPedrosoRei
import com.github.robfitzgerald.banditsearch.sampler.Sampler
import spire.algebra._
import spire.implicits._
import spire.math._

case class UCBPedrosoReiSampler[F[_] : Monad, S, A, V: Numeric](
  samplerState                : UCBPedrosoReiSamplerState[S, A, V],
  override val childrenOf     : S => Array[(Option[A], S)],
  override val simulate       : S => S,
  override val evaluate       : S => V,
  override val objective      : Objective[V],
  override val randomSelection: BanditParent[S, A, V] => BanditChild[S, A, V] = RandomSelection.complimentaryMultiplyWithCarry[S, A, V]
) extends Sampler[S, A, V] {
  // how can we curry the rewardFunction?
  // we will have new Cp values every algorithm iteration
  // we will possibly have new gBest/gWorst every sample
  // parentVisits is local to the BanditParent
  // when rewardFunction signatures are simple like this, Value => Reward, it allows for generic rewardFunctions with various dependencies
  // perhaps we need to add a MetaSearchNode.update() function along with the BanditParent/BanditChild .update() functions in Sampler.run()?
  // then sampler could be defined based on a MetaSearchNode instead of a BanditParent
  // ...
  // should this Sampler be also defined as the MetaSearchNode type, container for the Parent?
  val rewardFunction: V => Reward = {

    UCBPedrosoRei.rewardFunction[Double](???, ???, ???, ???, ???, ???)(???)

    ???
  }
}

object UCBPedrosoReiSampler {
  def initial[F[_] : Monad, S, A, V: Numeric](
    childrenOf: S => Array[(Option[A], S)],
    simulate  : S => S,
    evaluate  : S => V,
    objective : Objective[V],
    Cp        : Double = math.sqrt(2)
  ): UCBPedrosoReiSampler[F, S, A, V] = {
    val ucbPedrosoReiSamplerState = UCBPedrosoReiSamplerState[S, A, V](objective.badBounds, objective.optimalBounds, Cp)
    new UCBPedrosoReiSampler[F, S, A, V](ucbPedrosoReiSamplerState, childrenOf, simulate, evaluate, objective)
  }

  def promote[F[_], S, A, V: Numeric](
  ): UCBPedrosoReiSampler[F, S, A, V] = {
    ???
  }
}