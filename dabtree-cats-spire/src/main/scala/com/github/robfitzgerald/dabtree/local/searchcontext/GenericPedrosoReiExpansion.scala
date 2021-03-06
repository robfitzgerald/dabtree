package com.github.robfitzgerald.dabtree.local.searchcontext

import cats.implicits._
import cats.{Monad, MonoidK}

import com.github.robfitzgerald.dabtree.local.banditnode.{BanditChild, BanditParent}
import com.github.robfitzgerald.dabtree.common.banditnode.SearchState
import com.github.robfitzgerald.dabtree.local.Objective
import com.github.robfitzgerald.dabtree.local.sampler.pedrosorei.Payload
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
    generateChildren     : S => Array[(S, Option[A])]
  )(payload: Payload[S,A,V]): G[Payload[S,A,V]] = {

    val (banditParent, optionGlobals, random) = payload

    val allowExpansion = allowChildExpansion(banditParent.state)

    if (banditParent.mctsStats.observations < observationsThreshold) Monad[G].pure(payload)
    else if (!allowExpansion) {
      Monad[G].pure(payload)
    }
    else {
      optionGlobals match {
        case None => Monad[G].pure(payload)
        case Some(globals) =>

          // select expandable children, based on reward. promote them. retain their array index.
          val expandedChildren: Array[(BanditParent[S, A, V], Int)] =
            banditParent.
              children.
              zipWithIndex.
              filter { case (child: BanditChild[S, A, V], _) =>
                child.mctsStats.observations > observationsThreshold &&
                child.reward >= rewardThreshold
              }.
              sortBy { case (child: BanditChild[S, A, V], _) =>
                - child.reward
              }.
              take(maxExpandPerIteration).
              map { case (child: BanditChild[S, A, V], index: Int) =>
                (BanditChild.promote(child, evaluate, generateChildren, objective), index)
              }

          // remove expanded children from parent by array index
          val updatedParent: BanditParent[S, A, V] = {
            val updatedChildren: Array[BanditChild[S, A, V]] = {
              val temp = banditParent.children.toBuffer
              val childrenToRemove: Array[Int] =
                expandedChildren.
                  map { case (_, index: Int) => index }.
                  sortBy { -_ }
              for {
                expandedIndex <- childrenToRemove
              } {
                temp.remove(expandedIndex)
              }
              temp.toArray
            }

            // cancel the parent from searching if all of its children have been expanded
            if (updatedChildren.length == 0) {
              banditParent.copy(
                children = updatedChildren,
                searchState = SearchState.Cancelled
              )
            } else {
              banditParent.copy(
                children = updatedChildren
              )
            }
          }

          // re-package as payloads
          val expandedAsPayloads: Array[Payload[S,A,V]] =
            expandedChildren.map { case (expandedChild, _) => (expandedChild, Some(globals), random) }

          // combine with parent and return, wrapped in container type G
          expandedAsPayloads.foldLeft(Monad[G].pure((updatedParent, Option(globals), random))) { (acc, b) =>
            val rhs: G[Payload[S,A,V]] = Monad[G].pure(b)
            MonoidK[G].combineK(acc, rhs)
          }
      }
    }
  }

}
