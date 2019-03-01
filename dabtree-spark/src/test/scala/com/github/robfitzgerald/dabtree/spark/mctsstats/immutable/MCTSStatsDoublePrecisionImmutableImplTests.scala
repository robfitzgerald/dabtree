package com.github.robfitzgerald.dabtree.spark.mctsstats.immutable


import com.github.robfitzgerald.dabtree.DefaultTest
import com.github.robfitzgerald.dabtree.spark.objective.ObjectiveDoublePrecision.Minimize
import com.github.robfitzgerald.dabtree.spark.mctsstats.mutable.MCTSStatsMutableDoublePrecisionImpl

class MCTSStatsDoublePrecisionImmutableImplTests extends DefaultTest {
  "MCTSStatsImmutableImpl" when {
    "empty[Double]()" should {
      "correctly show an empty object" in {
        val (low, high) = (0D, 1000D)
        val empty: MCTSStatsImmutableDoublePrecisionImpl = MCTSStatsImmutableDoublePrecisionImpl.empty(Minimize(low, high))
        empty.min should equal (high)               // zero value for min operations
        empty.max should equal (low)                // zero value for max operations
        empty.mean should equal ((high + low) / 2)  // default value is midpoint
        empty.variance should equal (0D)
        empty.standardDeviation should equal (0D)
        empty.observations should equal (0)
      }
    }
    "empty[Double]().add(5.0D)" should {
      "correctly reflect one observation" in {
        val five: MCTSStatsImmutableDoublePrecisionImpl = MCTSStatsImmutableDoublePrecisionImpl.empty(Minimize(0D, 1000D)).update(5.0D)
        five.min should equal (5D)
        five.max should equal (5D)
        five.mean should equal (5D)
        five.variance should equal (0D)
        five.standardDeviation should equal (0D)
        five.observations should equal (1)
      }
    }
    ".toMutable" should {
      "not change the observed values" in {
        val twoObservations: MCTSStatsImmutableDoublePrecisionImpl = MCTSStatsImmutableDoublePrecisionImpl.empty(Minimize(0D, 1000D)).update(2.5D).update(7.5D)
        val asMutable: MCTSStatsMutableDoublePrecisionImpl = twoObservations.toMutable
        val expectedVariance: Double = (math.pow(2.5-5, 2) + math.pow(7.5-5, 2)) / 2
        asMutable.min should equal (2.5D)
        asMutable.max should equal (7.5D)
        asMutable.mean should equal (5D)
        asMutable.variance should equal (expectedVariance)
        asMutable.standardDeviation should equal (math.sqrt(expectedVariance))
        asMutable.observations should equal (2)
      }
    }
  }
}
