package com.github.robfitzgerald.dabtree.local.searchcontext

import scala.annotation.tailrec
import scala.collection.SortedSet

import cats.data.Chain
import cats.implicits._
import cats.{Foldable, Monad, MonoidK}

import com.github.robfitzgerald.dabtree.common.banditnode.SearchState
import com.github.robfitzgerald.dabtree.local.sampler.pedrosorei.Payload
import spire.math.Numeric

object GenericPedrosoReiPartitionRebalancing {

  type RankedPayload[S, A, V] = SortedSet[(Payload[S, A, V], Double)]

  def rebalance[G[_] : Monad : MonoidK : Foldable, S, A, V: Numeric](
    activatedPayloadLimit: Int,
    payloadCapacity      : Int,
    rankingPolicy        : Payload[S, A, V] => Double
  )(payloads: G[Payload[S, A, V]]): G[Payload[S, A, V]] = {


    implicit val ordering: Ordering[(Payload[S, A, V], Double)] = Ordering.by(-_._2)

    // evaluate all non-cancelled payloads in this partition, holding cancelled payloads aside
    val (nonCancelledRankedPayloads: RankedPayload[S, A, V], cancelledPayloads: Chain[Payload[S, A, V]]) =
      payloads.
        foldLeft((SortedSet.empty[(Payload[S, A, V], Double)], Chain.empty[Payload[S, A, V]])) { case (accumulator, payload) =>
          val (parent, _) = payload
          val (nonCancelled, cancelled) = accumulator
          parent.searchState match {
            case SearchState.Cancelled =>
              (nonCancelled, payload +: cancelled)
            case _ =>
              val ranked = (payload, rankingPolicy(payload))
              (nonCancelled + ranked, cancelled)
          }
        }

    // sort and re-label nonCancelled payloads
    val rebalancedNonCancelledPayloads: Chain[Payload[S, A, V]] = updateSearchNodeState[S, A, V](nonCancelledRankedPayloads, activatedPayloadLimit, payloadCapacity)

    // reassemble rebalanced nodes in original container type
    val result = (cancelledPayloads ++ rebalancedNonCancelledPayloads).foldLeft(MonoidK[G].empty[Payload[S, A, V]]) { (acc, b) =>
      val rhs: G[Payload[S, A, V]] = Monad[G].pure(b)
      MonoidK[G].combineK(acc, rhs)
    }

    result
  }

  def updateSearchNodeState[S, A, V: Numeric](
    nonCancelledPayloads : SortedSet[(Payload[S, A, V], Double)],
    activatedPayloadLimit: Int,
    payloadCapacity      : Int
  ): Chain[Payload[S, A, V]] = {

    // todo: sort and re-label nonCancelled payloads

    @tailrec
    def _update(sorted: SortedSet[(Payload[S, A, V], Double)], solution: Chain[Payload[S, A, V]] = Chain.empty[Payload[S, A, V]]): Chain[Payload[S, A, V]] = {
      if (solution.size == payloadCapacity) {

        // we have reached our active payload limit, so we must force the cancellation of any remaining payloads.
        val cancelledRemainder: Chain[Payload[S, A, V]] = {
          sorted.foldLeft(Chain.empty[Payload[S, A, V]]) { (acc, evaluatedPayload) =>
            val (parent, _) = evaluatedPayload._1
            val cancelledParent = parent.cancel()
            acc :+ (cancelledParent, None)
          }
        }
        solution ++ cancelledRemainder
      } else {
        sorted.headOption match {
          case None => solution
          case Some((payload: Payload[S, A, V], _: Double)) =>
            val (parent, globals) = payload
            val updatedParent =
              if (solution.size < activatedPayloadLimit) parent.activate()
              else parent.suspend()

            _update(sorted.tail, solution :+ (updatedParent, globals))
        }
      }
    }

    _update(nonCancelledPayloads)
  }
}
