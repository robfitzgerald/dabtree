package com.github.robfitzgerald.banditsearch

trait BanditSearch {

  type State

  type Action

  type Value

  type Reward = Double

  def simulate(state: State): State

  def evaluate(state: State): Value

  def rewardOf(value: Value): Reward

}
