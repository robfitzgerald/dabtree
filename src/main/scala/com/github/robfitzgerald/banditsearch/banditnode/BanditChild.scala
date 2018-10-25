package com.github.robfitzgerald.banditsearch.banditnode



import com.github.robfitzgerald.banditsearch.{Objective, SearchStats}
import com.github.robfitzgerald.banditsearch.mctsstats.implicits._
import com.github.robfitzgerald.banditsearch.mctsstats.immutable.MCTSStatsImmutableImpl
import spire.math.Numeric

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
case class BanditChild[S, A, V: Numeric](
  state        : S,
  action       : Option[A],
  var reward   : Double,
  var mctsStats: MCTSStatsImmutableImpl[V]
) extends BanditNode[S, A, V, Double] {
  def update(observation: V, rewardUpdate: Double): Unit = {
    reward = rewardUpdate
    mctsStats = mctsStats.update(observation)
  }
}

object BanditChild {

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
  def promote[S, A, V: Numeric](
    child                    : BanditChild[S, A, V],
    evaluate                 : Option[S => V],
    generateChildren         : S => Array[(S, Option[A])],
    objective                : Objective[V]
  ): BanditParent[S, A, V] = {

    val children: Array[BanditChild[S, A, V]] = generateChildren(child.state).map {toBanditChild[S, A, V](objective)}

    val uctBound: Option[V] = evaluate.map { fn => fn(child.state) }

    BanditParent(
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

  def toBanditChild [S, A, V : Numeric](objective: Objective[V])(payload: (S, Option[A])): BanditChild[S, A, V] = {
    val (childState, childAction) = payload
      BanditChild(
        childState,
        childAction,
        Double.PositiveInfinity,
        MCTSStatsImmutableImpl.empty[V](objective)
      )
  }

}