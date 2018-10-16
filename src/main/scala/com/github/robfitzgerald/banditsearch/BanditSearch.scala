package com.github.robfitzgerald.banditsearch

import com.github.robfitzgerald.banditsearch.banditnode.BanditParent


trait BanditSearch {

  type State

  type Action

  type Value

  type Reward = Double

  def simulate(state: State): State

  def evaluate(state: State): Value

  def rewardOf(value: Value): Reward

  def childrenOf(state: State): Array[(Option[Action], State)]

  def root(): BanditParent[State, Action, Value]
}
