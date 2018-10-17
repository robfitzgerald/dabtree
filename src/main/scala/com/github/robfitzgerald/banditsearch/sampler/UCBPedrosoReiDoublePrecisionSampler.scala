package com.github.robfitzgerald.banditsearch.sampler

import com.github.robfitzgerald.banditsearch.banditnode._
import com.github.robfitzgerald.banditsearch.randomselection.RandomSelection
import com.github.robfitzgerald.banditsearch.reward.UCBPedrosoRei


class UCBPedrosoReiDoublePrecisionSampler [S, A] (
  override val childrenOf: S => Array[(Option[A], S)],
  override val simulate: S => S,
  override val evaluate: S => Double,
  override val randomSelection: BanditParent[S, A, Double] => BanditChild[S, A, Double] = RandomSelection.complimentaryMultiplyWithCarry
) extends Sampler[S,A,Double] {
  val rewardFunction: (HasMCTSStats[Double], Double) => Reward = UCBPedrosoRei.apply[S, A, Double]
}
