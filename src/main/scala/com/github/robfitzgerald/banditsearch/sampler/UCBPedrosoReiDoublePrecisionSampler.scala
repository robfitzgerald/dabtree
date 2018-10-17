package com.github.robfitzgerald.banditsearch.sampler

import com.github.robfitzgerald.banditsearch.Objective
import com.github.robfitzgerald.banditsearch.banditnode._
import com.github.robfitzgerald.banditsearch.randomselection.RandomSelection
import com.github.robfitzgerald.banditsearch.reward.UCBPedrosoRei
import spire.implicits._


class UCBPedrosoReiDoublePrecisionSampler [S, A] (
  override val childrenOf: S => Array[(Option[A], S)],
  override val simulate: S => S,
  override val evaluate: S => Double,
  override val objective: Objective[Double] = Objective.Minimize(0D, Double.PositiveInfinity),
  override val randomSelection: BanditParent[S, A, Double] => BanditChild[S, A, Double] = RandomSelection.complimentaryMultiplyWithCarry
) extends Sampler[S,A,Double] {
  // how can we curry the rewardFunction?
  // we will have new Cp values every algorithm iteration
  // we will possibly have new gBest/gWorst every sample
  // parentVisits is local to the BanditParent
  // when rewardFunction signatures are simple like this, Value => Reward, it allows for generic rewardFunctions with various dependencies
  // perhaps we need to add a MetaSearchNode.update() function along with the BanditParent/BanditChild .update() functions in Sampler.run()?
  // then sampler could be defined based on a MetaSearchNode instead of a BanditParent
  // ...
  // should this Sampler be also defined as the MetaSearchNode type, container for the Parent?
  val rewardFunction: Double => Reward = UCBPedrosoRei.rewardFunction[Double]()
}
