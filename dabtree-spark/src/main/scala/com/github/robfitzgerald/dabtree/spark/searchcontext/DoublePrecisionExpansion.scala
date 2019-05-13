package com.github.robfitzgerald.dabtree.spark.searchcontext

import cats.implicits._
import cats.{Monad, MonoidK}

import com.github.robfitzgerald.dabtree.common.banditnode.SearchState
import com.github.robfitzgerald.dabtree.spark.banditnode.{SparkBanditChild, SparkBanditParent}
import com.github.robfitzgerald.dabtree.spark.objective.Objective
import com.github.robfitzgerald.dabtree.spark.sampler.pedrosorei.Payload

object DoublePrecisionExpansion {

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
    * @return
    */
  def expand[G[_] : Monad : MonoidK, S, A](
    observationsThreshold: Int,
    rewardThreshold: Double,
    maxExpandPerIteration: Int,
    objective            : Objective[Double],
    allowChildExpansion  : S => Boolean,
    evaluate             : Option[S => Double],
    generateChildren     : S => Array[(S, Option[A])]
  )(payload: Payload[S,A,Double]): G[Payload[S,A,Double]] = {

    val (banditParent, optionGlobals, stopTime, random) = payload

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
          val expandedChildren: Array[(SparkBanditParent[S, A], Int)] =
            banditParent.
              children.
              zipWithIndex.
              filter { case (child: SparkBanditChild[S, A], _) =>
                child.mctsStats.observations > observationsThreshold &&
                child.reward >= rewardThreshold
              }.
              sortBy { case (child: SparkBanditChild[S, A], _) =>
                - child.reward
              }.
              take(maxExpandPerIteration).
              map { case (child: SparkBanditChild[S, A], index: Int) =>
                (SparkBanditChild.promote(child, evaluate, generateChildren, objective), index)
              }

          // remove expanded children from parent by array index
          val updatedParent: SparkBanditParent[S, A] = {
            val updatedChildren: Array[SparkBanditChild[S, A]] = {
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
          val expandedAsPayloads: Array[Payload[S,A,Double]] =
            expandedChildren.map { case (expandedChild, _) => (expandedChild, Some(globals), stopTime, random) }

          // combine with parent and return, wrapped in container type G
          val result = expandedAsPayloads.foldLeft(Monad[G].pure((updatedParent, Option(globals), stopTime, random))) { (acc, b) =>
            val rhs: G[Payload[S,A,Double]] = Monad[G].pure(b)
            MonoidK[G].combineK(acc, rhs)
          }

          result
      }
    }
  }

}
