package com.github.robfitzgerald.dabtree.spark.searchcontext

import com.github.robfitzgerald.dabtree.DefaultTest
import cats.implicits._

import com.github.robfitzgerald.dabtree.common.SearchStats
import com.github.robfitzgerald.dabtree.common.banditnode.SearchState
import com.github.robfitzgerald.dabtree.spark.banditnode.{SparkBanditChild, SparkBanditParent}
import com.github.robfitzgerald.dabtree.spark.mctsstats.immutable.MCTSStatsImmutableDoublePrecisionImpl
import com.github.robfitzgerald.dabtree.spark.objective.ObjectiveDoublePrecision
import com.github.robfitzgerald.dabtree.spark.sampler.pedrosorei.{Payload, UCBPedrosoReiGlobals}

class DoublePrecisionRebalancingTests extends DefaultTest {
  import DoublePrecisionRebalancingTests._
  "rebalancing" when {
    "when called with too many Active payloads" should {
      "suspend some" in {
        val activePayloadLimit = 5
        val totalPayloadLimit = 10
        val result = DoublePrecisionRebalancing.rebalance[List, TestState, TestAction](activePayloadLimit, totalPayloadLimit, payload => payload._1.reward)(problem)
        result.count(_._1.searchState == SearchState.Activated) should equal (5)
        result.count(_._1.searchState == SearchState.Suspended) should equal (5)
        val (act, sus) = result.partition(_._1.searchState == SearchState.Activated)
        for {
          suspendedNode <- sus.map{_._1}
          activeNode <- act.map{_._1}
        } {
          suspendedNode.reward should be < activeNode.reward
        }
      }
    }
    "when called with too many Active and Suspended payloads" should {
      "suspend and cancel some" in {
        val activePayloadLimit = 3
        val totalPayloadLimit = 6
        val result = DoublePrecisionRebalancing.rebalance[List, TestState, TestAction](activePayloadLimit, totalPayloadLimit, payload => payload._1.reward)(problem)
        result.count(_._1.searchState == SearchState.Activated) should equal (3)
        result.count(_._1.searchState == SearchState.Suspended) should equal (3)
        result.count(_._1.searchState == SearchState.Cancelled) should equal (4)
        val (act, sus) = result.partition(_._1.searchState == SearchState.Activated)
        for {
          suspendedNode <- sus.map{_._1}
          activeNode <- act.map{_._1}
        } {
          suspendedNode.reward should be < activeNode.reward
        }
      }
    }
  }
}

object DoublePrecisionRebalancingTests {
  type TestState = Array[Int]
  type TestAction = Int
  type TestPayload = Payload[TestState, TestAction, Double]
  val probSize = 10
  val problem: List[TestPayload] = {
    for {
      index <- 1 to probSize
    } yield {
      // dummy data. only SearchState and reward is relevant here.
      val parent = SparkBanditParent(
        SearchState.Activated,
        Array.empty[Int],
        Option.empty[Int],
        index.toDouble / probSize,
        MCTSStatsImmutableDoublePrecisionImpl.empty(ObjectiveDoublePrecision.Minimize(0, 1)),
        Array.empty[SparkBanditChild[TestState, TestAction]],
        SearchStats(),
        None
      )
      (parent, Option.empty[UCBPedrosoReiGlobals[TestState, TestAction, Double]], 0L)
    }
  }.toList
}