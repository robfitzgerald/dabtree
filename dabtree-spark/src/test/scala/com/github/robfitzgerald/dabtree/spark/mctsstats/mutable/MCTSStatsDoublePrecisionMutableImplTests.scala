package com.github.robfitzgerald.dabtree.spark.mctsstats.mutable

import com.github.robfitzgerald.dabtree.DefaultTest
import com.github.robfitzgerald.dabtree.spark.mctsstats.immutable.MCTSStatsImmutableDoublePrecisionImpl
import com.github.robfitzgerald.dabtree.spark.objective.ObjectiveDoublePrecision.Minimize

class MCTSStatsDoublePrecisionMutableImplTests extends DefaultTest {
  "MCTSStatsMutableImpl" when {
    "empty()" should {
      "correctly show an empty object" in {
        val (low, high) = (0D, 1000D)
        val empty: MCTSStatsMutableDoublePrecisionImpl = MCTSStatsMutableDoublePrecisionImpl.empty(Minimize(low, high))
        empty.min should equal (high)               // zero value for min operations
        empty.max should equal (low)                // zero value for max operations
        empty.mean should equal ((high + low) / 2)  // default value is midpoint
        empty.variance should equal (0D)
        empty.standardDeviation should equal (0D)
        empty.observations should equal (0)
      }
    }
    "empty().add(5.0D)" should {
      "correctly reflect one observation" in {
        val five: MCTSStatsMutableDoublePrecisionImpl = MCTSStatsMutableDoublePrecisionImpl.empty(Minimize(0D, 1000D))
        five.update(5.0D)
        five.min should equal (5D)
        five.max should equal (5D)
        five.mean should equal (5D)
        five.variance should equal (0D)
        five.standardDeviation should equal (0D)
        five.observations should equal (1)
      }
    }
    "empty().add(2.5D).add(7.5D)" should {
      "correctly reflect two observations" in {
        val twoObservations: MCTSStatsMutableDoublePrecisionImpl = MCTSStatsMutableDoublePrecisionImpl.empty(Minimize(0D, 1000D))
        twoObservations.update(2.5D)
        twoObservations.update(7.5D)
        val expectedVariance: Double = (math.pow(2.5-5, 2) + math.pow(7.5-5, 2)) / 2
        twoObservations.min should equal (2.5D)
        twoObservations.max should equal (7.5D)
        twoObservations.mean should equal (5D)
        twoObservations.variance should equal (expectedVariance)
        twoObservations.standardDeviation should equal (math.sqrt(expectedVariance))
        twoObservations.observations should equal (2)
      }
    }
    "when created from an immutable stats collection" should {
      "still evaluate as expected" in {
        val (low, high) = (0D, 1000D)
        val imm: MCTSStatsImmutableDoublePrecisionImpl = MCTSStatsImmutableDoublePrecisionImpl.empty(Minimize(low, high))
        val mut: MCTSStatsMutableDoublePrecisionImpl = imm.toMutable
        mut.update(2.5D)
        mut.update(7.5D)
        val expectedVariance: Double = (math.pow(2.5-5, 2) + math.pow(7.5-5, 2)) / 2
        mut.min should equal (2.5D)
        mut.max should equal (7.5D)
        mut.mean should equal (5D)
        mut.variance should equal (expectedVariance)
        mut.standardDeviation should equal (math.sqrt(expectedVariance))
        mut.observations should equal (2)
      }
    }
    ".toImmutable" should {
      "not change the observed values" in {
        val twoObservations: MCTSStatsMutableDoublePrecisionImpl = MCTSStatsMutableDoublePrecisionImpl.empty(Minimize(0D, 1000D))
        twoObservations.update(2.5D)
        twoObservations.update(7.5D)
        val asImmutable: MCTSStatsImmutableDoublePrecisionImpl = twoObservations.toImmutable
        val expectedVariance: Double = (math.pow(2.5-5, 2) + math.pow(7.5-5, 2)) / 2
        asImmutable.min should equal (2.5D)
        asImmutable.max should equal (7.5D)
        asImmutable.mean should equal (5D)
        asImmutable.variance should equal (expectedVariance)
        asImmutable.standardDeviation should equal (math.sqrt(expectedVariance))
        asImmutable.observations should equal (2)
      }
    }
  }
}
