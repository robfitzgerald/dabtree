package com.github.robfitzgerald.banditsearch.banditnode

import com.github.robfitzgerald.banditsearch.{Objective, SearchStats}
import com.github.robfitzgerald.banditsearch.mctsstats.immutable.MCTSStatsImmutableImpl
import com.github.robfitzgerald.banditsearch.mctsstats.implicits._
import spire.math._

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
case class BanditParent[S, A, V: Numeric](
  searchState              : SearchState,
  state                    : S,
  action                   : Option[A],
  var reward               : Double,
  var mctsStats: MCTSStatsImmutableImpl[V],
  children                 : Array[BanditChild[S, A, V]],
  searchStats              : SearchStats,
  costBound                : Option[V]
) extends BanditNode[S, A, V, Double] with HasChildren[S, A, V, Double] {
  override type Child = BanditChild[S, A, V]

  def activate(): BanditParent[S, A, V] = {
    this.copy(
      searchState = SearchState.Activated,
      searchStats = this.searchStats.copy(
        activated = this.searchStats.activated + 1
      )
    )
  }

  def suspend(): BanditParent[S, A, V] = {
    this.copy(
      searchState = SearchState.Suspended,
      searchStats = this.searchStats.copy(
        suspended = this.searchStats.suspended + 1
      )
    )
  }

  def cancel(): BanditParent[S, A, V] = {
    this.copy(
      searchState = SearchState.Cancelled,
      searchStats = this.searchStats.copy(
        cancelled = this.searchStats.cancelled + 1
      )
    )
  }

  def update(observation: V, rewardUpdate: Double): Unit = {
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

object BanditParent {
  def frontierPayload[S, A, V: Numeric](
    state: S,
    action: Option[A],
    evaluate: Option[S => V],
    generateChildren: S => Array[(S, Option[A])],
    objective: Objective[V]
  ): BanditParent[S, A, V] = {

    val children = generateChildren(state).map {
      BanditChild.toBanditChild(objective)
    }
    val uctBound: Option[V] = evaluate.map { fn => fn(state) }

    BanditParent(
      SearchState.Activated,
      state,
      action,
      Double.PositiveInfinity,
      MCTSStatsImmutableImpl.empty(objective),
      children,
      SearchStats(),
      uctBound
    )
  }
}