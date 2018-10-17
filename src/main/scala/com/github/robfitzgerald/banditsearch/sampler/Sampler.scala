package com.github.robfitzgerald.banditsearch.sampler

import com.github.robfitzgerald.banditsearch.Objective
import com.github.robfitzgerald.banditsearch.banditnode.{BanditChild, BanditParent, HasMCTSStats}
import spire.implicits._


trait Sampler [State, Action, Value] {

  type Reward = Double
  type Parent = BanditParent[State, Action, Value]
  type Child = BanditChild[State, Action, Value]

  def objective: Objective[Value]

  def randomSelection: Parent => Child

  def simulate: State => State

  def evaluate: State => Value

  def childrenOf: State => Array[(Option[Action], State)]

  def rewardFunction: (HasMCTSStats[Value], Value, Int) => Reward

  def run(parent: Parent, iterations: Int): Unit = {
    cfor(0)(_ < iterations, _ + 1) { _ =>

      val selectedChild: Child = randomSelection(parent)
      val simulation: State = simulate(selectedChild.state)
      val cost: Value = evaluate(simulation)

      selectedChild.update(cost, rewardFunction(selectedChild, cost, parent.mctsStats.observations))
      parent.update(cost, rewardFunction(parent, cost, 0))
    }
  }
}