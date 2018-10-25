package com.github.robfitzgerald.banditsearch.searchcontext

import cats.implicits._
import cats.{Monad, MonoidK}

import com.github.robfitzgerald.banditsearch.Objective
import com.github.robfitzgerald.banditsearch.banditnode.{BanditChild, BanditParent, SearchState}
import com.github.robfitzgerald.banditsearch.pedrosorei.Payload
import spire.math.Numeric

object GenericPedrosoReiExpansion {

  /**
    * expands children nodes into parent nodes based on user-provided parameters
    *
    * @param observationsThreshold number of observations required (parent and child) before expansion occurs.
    * @param rewardThreshold the reward value a child must exceed to be considered for expansion.
    * @param maxExpandPerIteration a limit of promotions from child to parent that can occur in a single call to this function.
    * @param allowChildExpansion a user-defined predicate to limit the depth into the search space that expansion is allowed.
    * @param evaluate an (optional) user-defined function that computes the cost of a state. used to compute the cost lower bounds of a node.
    * @param generateChildren a user-defined function that lists all possible action-state tuples of a parent state.
    * @param payload the payload to possibly expand on.
    * @tparam G the container type used for the search algorithm to store payloads
    * @tparam S user-provided State type
    * @tparam A user-provided Action type
    * @tparam V user-provided cost value type
    * @return
    */
  def expand[G[_] : Monad : MonoidK, S, A, V: Numeric](
    observationsThreshold: Int,
    rewardThreshold: Double,
    maxExpandPerIteration: Int,
    objective            : Objective[V],
    allowChildExpansion  : S => Boolean,
    evaluate             : Option[S => V],
    generateChildren     : S => Array[(S, Option[A])],
  )(payload: Payload[S,A,V]): G[Payload[S,A,V]] = {
    if (payload._1.mctsStats.observations < observationsThreshold) Monad[G].pure(payload)
    else if (!allowChildExpansion(payload._1.state)) Monad[G].pure(payload)
    else {
      payload._2 match {
        case None => Monad[G].pure(payload)
        case Some(globals) =>

          // select expandable children, based on reward. promote them. retain their array index.
          val expandedChildren: Array[(BanditParent[S, A, V], Int)] =
            payload._1.
              children.
              zipWithIndex.
              filter { case (child: BanditChild[S, A, V], _) =>
                child.mctsStats.observations > observationsThreshold &&
                child.reward >= rewardThreshold
              }.
              sortBy {
                -_._1.reward
              }.
              take(maxExpandPerIteration).
              map { case (child: BanditChild[S, A, V], index: Int) =>
                (BanditChild.promote(child, evaluate, generateChildren, objective), index)
              }

          // remove expanded children from parent by array index
          val updatedParent: BanditParent[S, A, V] = {
            val updatedChildren: Array[BanditChild[S, A, V]] = {
              val temp = payload._1.children.toBuffer
              for {
                expandedIndex <- expandedChildren.map { case (_, index: Int) => index }.reverse
              } {
                temp.remove(expandedIndex)
              }
              temp.toArray
            }

            // cancel the parent from searching if all of its children have been expanded
            if (updatedChildren.length == 0) {
              payload._1.copy(
                children = updatedChildren,
                searchState = SearchState.Cancelled
              )
            } else {
              payload._1.copy(
                children = updatedChildren
              )
            }
          }

          // re-package as payloads
          val expandedAsPayloads: Array[Payload[S,A,V]] =
            expandedChildren.map { case (expandedChild, _) => (expandedChild, Some(globals)) }

          // combine with parent and return, wrapped in container type G
          expandedAsPayloads.foldLeft(Monad[G].pure((updatedParent, Option(globals)))) { (acc, b) =>
            val rhs: G[Payload[S,A,V]] = Monad[G].pure(b)
            MonoidK[G].combineK(acc, rhs)
          }
      }
    }
  }

}
