package com.github.robfitzgerald.banditsearch.banditnode

import com.github.robfitzgerald.banditsearch.SearchStats
import com.github.robfitzgerald.banditsearch.mctsstats.immutable.MCTSStatsImmutableImpl
import spire.math._

/**
  * represents the parent in a Multi-Armed UCB Bandit Search, with value type precision of Double
  * @param searchState this node is one of {Activated,Suspended,Cancelled} as assigned by the search algorithm
  * @param state the (partial) state in the state-space exploration, represented by this node
  * @param action the action (if any) applied to produce this node
  * @param reward the expected long-term reward value associated with this node, computed by UCB, which should converge to a value in the range [0,1]
  * @param mctsStats simulation observation statistics, in Double precision
  * @param children any child states we are exploring from this node
  * @param searchStats a count of node search state changes
  * @param uctExplorationCoefficient UCB coefficient defining exploitation/exploration trade-off
  * @param costBound the (optional) computed known constraint on costs of this and any children, in problems where costs are monotonic
  * @tparam S user-provided State type
  * @tparam A user-provided Action type
  */
case class BanditParent [S,A,V : Fractional] (
  searchState              : SearchState,
  state                    : S,
  action                   : Option[A],
  var reward               : Double,
  var mctsStats            : MCTSStatsImmutableImpl[V],
  children                 : Array[BanditChild[S,A,V]],
  searchStats              : SearchStats,
  uctExplorationCoefficient: V,
  costBound                : Option[V]
) extends BanditNode[S,A,V,Double] with HasChildren[S,A,V,Double] {
  override type Child = BanditChild[S,A,V]

  def update(observation: V, rewardUpdate: Double): Unit = {
    reward = rewardUpdate
    mctsStats = mctsStats.add(observation)
  }
}

object BanditParent {

}