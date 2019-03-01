package com.github.robfitzgerald.dabtree.spark.banditnode

import com.github.robfitzgerald.dabtree.common.banditnode.{BanditNode, HasChildren, SearchState}
import com.github.robfitzgerald.dabtree.common.SearchStats
import com.github.robfitzgerald.dabtree.spark.mctsstats.immutable.MCTSStatsImmutableDoublePrecisionImpl
import com.github.robfitzgerald.dabtree.spark.objective.Objective

/**
  * represents the parent in a Multi-Armed UCB Bandit Search, with value type precision of Double
  *
  * @param searchState this node is one of {Activated,Suspended,Cancelled} as assigned by the search algorithm
  * @param state       the (partial) state in the state-space exploration, represented by this node
  * @param action      the action (if any) applied to produce this node
  * @param reward      the expected long-term reward value associated with this node, computed by UCB, which should converge to a value in the range [0,1]
  * @param mctsStats   simulation observation statistics, in Double precision
  * @param children    any child states we are exploring from this node
  * @param searchStats a count of node search state changes
  * @param costBound   the (optional) computed known constraint on costs of this and any children, in problems where costs are monotonic
  * @tparam S user-provided State type
  * @tparam A user-provided Action type
  */
case class SparkBanditParent[S, A](
  searchState              : SearchState,
  state                    : S,
  action                   : Option[A],
  var reward               : Double,
  var mctsStats: MCTSStatsImmutableDoublePrecisionImpl,
  children                 : Array[SparkBanditChild[S, A]],
  searchStats              : SearchStats,
  costBound                : Option[Double]
) extends BanditNode[S, A, Double, Double] with HasChildren[S, A, Double, Double] {
  override type Child = SparkBanditChild[S, A]

  def activate(): SparkBanditParent[S, A] = {
    this.copy(
      searchState = SearchState.Activated,
      searchStats = this.searchStats.copy(
        activated = this.searchStats.activated + 1
      )
    )
  }

  def suspend(): SparkBanditParent[S, A] = {
    this.copy(
      searchState = SearchState.Suspended,
      searchStats = this.searchStats.copy(
        suspended = this.searchStats.suspended + 1
      )
    )
  }

  def cancel(): SparkBanditParent[S, A] = {
    this.copy(
      searchState = SearchState.Cancelled,
      searchStats = this.searchStats.copy(
        cancelled = this.searchStats.cancelled + 1
      )
    )
  }

  def update(observation: Double, rewardUpdate: Double): Unit = {
    reward = rewardUpdate
    mctsStats = mctsStats.update(observation)
  }

  override def toString: String = {
    costBound match {
      case None =>
        f"$searchState node with state: $state - num children: ${children.length} - reward $reward\n$mctsStats"
      case Some(costBoundValue) =>
        f"$searchState with state: $state - cost bound: $costBoundValue - num children: ${children.length} - reward $reward\n$mctsStats"
    }
  }
}

object SparkBanditParent {
  def frontierPayload[S, A](
    state: S,
    action: Option[A],
    evaluate: Option[S => Double],
    generateChildren: S => Array[(S, Option[A])],
    objective: Objective[Double]
  ): SparkBanditParent[S, A] = {

    val children = generateChildren(state).map {
      SparkBanditChild.toBanditChild(objective)
    }
    val uctBound: Option[Double] = evaluate.map { fn => fn(state) }

    SparkBanditParent(
      SearchState.Activated,
      state,
      action,
      Double.PositiveInfinity,
      MCTSStatsImmutableDoublePrecisionImpl.empty(objective),
      children,
      SearchStats(),
      uctBound
    )
  }
}