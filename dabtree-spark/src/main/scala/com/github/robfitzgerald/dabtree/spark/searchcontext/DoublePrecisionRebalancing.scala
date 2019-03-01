package com.github.robfitzgerald.dabtree.spark.searchcontext

import scala.annotation.tailrec
import scala.collection.SortedSet

import cats.data.Chain
import cats.implicits._
import cats.{Foldable, Monad, MonoidK}

import com.github.robfitzgerald.dabtree.common.banditnode.SearchState
import com.github.robfitzgerald.dabtree.spark.sampler.pedrosorei.Payload

object DoublePrecisionRebalancing {

  type RankedPayload[S, A, Double] = SortedSet[(Payload[S, A, Double], Double)]

  def rebalance[G[_] : Monad : MonoidK : Foldable, S, A](
    activatedPayloadLimit: Int,
    payloadCapacity      : Int,
    rankingPolicy        : Payload[S, A, Double] => Double
  )(payloads: G[Payload[S, A, Double]]): G[Payload[S, A, Double]] = {


    implicit val ordering: Ordering[(Payload[S, A, Double], Double)] = Ordering.by(-_._2)

    // evaluate all non-cancelled payloads in this partition, holding cancelled payloads aside
    val (nonCancelledRankedPayloads: RankedPayload[S, A, Double], cancelledPayloads: Chain[Payload[S, A, Double]]) =
      payloads.
        foldLeft((SortedSet.empty[(Payload[S, A, Double], Double)], Chain.empty[Payload[S, A, Double]])) { case (accumulator, payload) =>
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
    val rebalancedNonCancelledPayloads: Chain[Payload[S, A, Double]] = updateSearchNodeState[S, A, Double](nonCancelledRankedPayloads, activatedPayloadLimit, payloadCapacity)

    // reassemble rebalanced nodes in original container type
    val result = (cancelledPayloads ++ rebalancedNonCancelledPayloads).foldLeft(MonoidK[G].empty[Payload[S, A, Double]]) { (acc, b) =>
      val rhs: G[Payload[S, A, Double]] = Monad[G].pure(b)
      MonoidK[G].combineK(acc, rhs)
    }

    result
  }

  def updateSearchNodeState[S, A, V: Numeric](
    nonCancelledPayloads : SortedSet[(Payload[S, A, Double], Double)],
    activatedPayloadLimit: Int,
    payloadCapacity      : Int
  ): Chain[Payload[S, A, Double]] = {

    // todo: sort and re-label nonCancelled payloads

    @tailrec
    def _update(sorted: SortedSet[(Payload[S, A, Double], Double)], solution: Chain[Payload[S, A, Double]] = Chain.empty[Payload[S, A, Double]]): Chain[Payload[S, A, Double]] = {
      if (solution.size == payloadCapacity) {

        // we have reached our active payload limit, so we must force the cancellation of any remaining payloads.
        val cancelledRemainder: Chain[Payload[S, A, Double]] = {
          sorted.foldLeft(Chain.empty[Payload[S, A, Double]]) { (acc, evaluatedPayload) =>
            val (parent, _) = evaluatedPayload._1
            val cancelledParent = parent.cancel()
            acc :+ (cancelledParent, None)
          }
        }
        solution ++ cancelledRemainder
      } else {
        sorted.headOption match {
          case None => solution
          case Some((payload: Payload[S, A, Double], _: Double)) =>
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
