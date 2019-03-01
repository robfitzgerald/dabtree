package com.github.robfitzgerald.dabtree.spark.banditnode

import com.github.robfitzgerald.dabtree.common.banditnode.{BanditNode, SearchState}
import com.github.robfitzgerald.dabtree.common.SearchStats
import com.github.robfitzgerald.dabtree.spark.mctsstats.immutable.MCTSStatsImmutableDoublePrecisionImpl
import com.github.robfitzgerald.dabtree.spark.objective.Objective

/**
  * represents the child in a Multi-Armed Bandit UCB Search
  *
  * @param state     the (partial) state in the state-space exploration, represented by this node
  * @param action    the action (if any) applied to produce this node
  * @param reward    the expected long-term reward value associated with this node, computed by UCB, which should converge to a value in the range [0,1]
  * @param mctsStats simulation observation statistics, in Double precision
  * @tparam S user-provided State type
  * @tparam A user-provided Action type
  */
case class SparkBanditChild[S, A](
  state        : S,
  action       : Option[A],
  var reward   : Double,
  var mctsStats: MCTSStatsImmutableDoublePrecisionImpl
) extends BanditNode[S, A, Double, Double] {

  def update(observation: Double, rewardUpdate: Double): Unit = {
    reward = rewardUpdate
    mctsStats = mctsStats.update(observation)
  }
}

object SparkBanditChild {

  /**
    * promotes a Child to a Parent
    *
    * @param child                     the child to promote
    * @param evaluate                  function to evaluate the uctBound, or none to leave un-calculated, which will ignore any operations on this uctBound
    * @param generateChildren          function that produces all possible child action/state tuples for this state
    * @tparam S user-provided State type
    * @tparam A user-provided Action type
    * @return a child node promoted to a BanditParentRegular node
    */
  def promote[S, A, V](
    child                    : SparkBanditChild[S, A],
    evaluate                 : Option[S => Double],
    generateChildren         : S => Array[(S, Option[A])],
    objective                : Objective[Double]
  ): SparkBanditParent[S, A] = {

    val children: Array[SparkBanditChild[S, A]] = generateChildren(child.state).map {toBanditChild[S, A](objective)}

    val uctBound: Option[Double] = evaluate.map { fn => fn(child.state) }

    SparkBanditParent(
      SearchState.Activated,
      child.state,
      child.action,
      child.reward,
      child.mctsStats,
      children,
      SearchStats(),
      uctBound
    )
  }

  def toBanditChild [S, A](objective: Objective[Double])(payload: (S, Option[A])): SparkBanditChild[S, A] = {
    val (childState, childAction) = payload
      SparkBanditChild(
        childState,
        childAction,
        Double.PositiveInfinity,
        MCTSStatsImmutableDoublePrecisionImpl.empty(objective)
      )
  }

}