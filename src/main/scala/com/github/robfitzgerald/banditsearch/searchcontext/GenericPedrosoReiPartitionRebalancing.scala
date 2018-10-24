package com.github.robfitzgerald.banditsearch.searchcontext

import scala.collection.SortedSet

import cats.data.Chain
import cats.implicits._
import cats.{Foldable, Monad, MonoidK}

import com.github.robfitzgerald.banditsearch.banditnode.SearchState
import com.github.robfitzgerald.banditsearch.pedrosorei.Payload
import spire.math.Numeric

object GenericPedrosoReiPartitionRebalancing {

  type RankedPayload[S,A,V] = SortedSet[(Payload[S,A,V], Double)]

  def rebalance[G[_] : Monad : MonoidK : Foldable, S, A, V: Numeric](
    activatedPayloadLimit: Int,
    payloadCapacity: Int,
    rankingPolicy: Payload[S,A,V] => Double
  )(payloads: G[Payload[S, A, V]]): G[Payload[S, A, V]] = {



    implicit val ordering: Ordering[(Payload[S,A,V], Double)] = Ordering.by(-_._2)

    // evaluate all non-cancelled payloads in this partition, holding cancelled payloads aside
    val (nonCancelledRankedPayloads: RankedPayload[S,A,V], cancelledPayloads: Chain[Payload[S,A,V]]) =
      payloads.
        foldLeft((SortedSet.empty[(Payload[S,A,V], Double)],Chain.empty[Payload[S,A,V]])) { case (accumulator, payload) =>
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
    val rebalancedNonCancelledPayloads: Chain[Payload[S,A,V]] = updateSearchNodeState[S,A,V](nonCancelledRankedPayloads, activatedPayloadLimit, payloadCapacity)

    // reassemble rebalanced nodes in original container type
    (cancelledPayloads ++ rebalancedNonCancelledPayloads).foldLeft(MonoidK[G].empty[Payload[S,A,V]]) { (acc, b) =>
      val rhs: G[Payload[S,A,V]] = Monad[G].pure(b)
      MonoidK[G].combineK(acc, rhs)
    }
  }

  def updateSearchNodeState[S,A,V](
    nonCancelledPayloads: SortedSet[(Payload[S,A,V], Double)],
    activatedPayloadLimit: Int,
    payloadCapacity: Int
  ): Chain[Payload[S,A,V]] = {

    // todo: sort and re-label nonCancelled payloads
    // taken from SO-Routing:

//    def updateUCTSearchNodeState(evaluated: Seq[BanditSearchPayload], activePayloadLimit: Int, noncancelledPayloadLimit: Int): Seq[BanditSearchPayload] = {
//
//      @tailrec
//      def update(index: Int = 0, solution: List[BanditSearchPayload] = List()): List[BanditSearchPayload] = {
//        if (index == evaluated.size) solution
//        else {
//          val thisPayload: BanditSearchPayload = evaluated(index)
//          val thisNode: UCTSearch = thisPayload.uctSearch
//          val updatedNode: UCTSearch =
//            if (index < activePayloadLimit) thisNode.asActive
//            else if (index < noncancelledPayloadLimit) thisNode.asSuspended
//            else thisNode.asCancelled
//          val updatedPayload: BanditSearchPayload = thisPayload.copy(uctSearch = updatedNode)
//          update(index + 1, updatedPayload +: solution)
//        }
//      }
//
//      update()
//    }
    ???
  }
}
