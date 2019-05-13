package com.github.robfitzgerald.dabtree.spark.sampler

import com.github.robfitzgerald.dabtree.spark.banditnode.{SparkBanditChild, SparkBanditParent}
import com.github.robfitzgerald.dabtree.spark.mctsstats.HasStats
import com.github.robfitzgerald.dabtree.spark.mctsstats.mutable.MCTSStatsMutableDoublePrecisionImpl
import com.github.robfitzgerald.dabtree.spark.objective.Objective

/**
  * describes a sampler data structure's types and functions on those types
  * @tparam State the user's state representation
  * @tparam Action the user's action representation
  * @tparam Value the user's chosen numeric type. allows for optimizing runtime vs accuracy
  * @tparam Globals a type that holds general global data associated with the search
  */
trait CanSample [State, Action, Value, Globals] {

  //////////////////////////////////////////////////////////////////////////////
  // user-provided members. these provide the problem domain we are searching in

//  type Reward = Double

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

  //////////////////////////////////////////////////////////////////////////
  // library-provided members. provided by classes that implement CanSample.

  type Parent = SparkBanditParent[State, Action]
  type Child = SparkBanditChild[State, Action]

  /**
    * a Minimize or Maximize objective for this search
    * @return our objective
    */
  protected def objective: Objective[Value]

  /**
    * update in-place the stats based on an observation
    * @return ()
    */
  def updateStats(stats: MCTSStatsMutableDoublePrecisionImpl, observation: Value): Unit

  /**
    * update in-place the global sampler state
    * @return ()
    */
  def updateSamplerState: (Globals, State, Action, Value) => Globals

  /**
    * computes the reward based on the current stats, global variables, and parent observations
    * @return a reward value for this node.
    */
  def rewardFunction(stats: HasStats, globals: Globals, pVisits: Int): Double
}