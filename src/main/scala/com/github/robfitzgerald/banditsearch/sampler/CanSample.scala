package com.github.robfitzgerald.banditsearch.sampler

import com.github.robfitzgerald.banditsearch.Objective
import com.github.robfitzgerald.banditsearch.banditnode.{BanditChild, BanditParent}
import com.github.robfitzgerald.banditsearch.mctsstats.mutable.MCTSStatsMutableImpl

/**
  * describes a sampler data structure's types and functions on those types
  * @tparam State the user's state representation
  * @tparam Action the user's action representation
  * @tparam Value the user's chosen numeric type. allows for optimizing runtime vs accuracy
  * @tparam SamplerState a type that holds general global data associated with the search
  */
trait CanSample [State, Action, Value, SamplerState] {

  type Reward = Double
  type Parent = BanditParent[State, Action, Value]
  type Child = BanditChild[State, Action, Value]

  /**
    * selects the index of a random child
    * @return an int index
    */
  def randomSelection: Parent => Int

  /**
    * given a state, simulate state transitions to a terminal state
    * @return the terminal state, based on the user's state traversal algorithm
    */
  def simulate: State => State

  /**
    * evaluate the cost of the terminal state
    * @return a cost evaluation with the user's provided cost function
    */
  def evaluate: State => Value

  /**
    * the state of the search. holds global variables.
    * @return a sampler state object
    */
  protected def samplerState: SamplerState

  /**
    * a Minimize or Maximize objective for this search
    * @return our objective
    */
  protected def objective: Objective[Value]

  /**
    * update in-place the stats based on an observation
    * @return ()
    */
  def updateStats: (MCTSStatsMutableImpl[Value], Value) => Unit

  /**
    * update in-place the global sampler state
    * @return ()
    */
  def updateSamplerState: (SamplerState, Value) => Unit

  /**
    * computes the reward based on the current stats, global variables, and parent observations
    * @return a reward value for this node.
    */
  def rewardFunction: (MCTSStatsMutableImpl[Value], SamplerState, Int) => Reward
}