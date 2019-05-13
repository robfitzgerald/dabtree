package com.github.robfitzgerald.dabtree.local.mctsstats.mutable

import com.github.robfitzgerald.dabtree.DefaultTest
import spire.implicits._
import com.github.robfitzgerald.dabtree.local.Objective.Minimize
import com.github.robfitzgerald.dabtree.local.mctsstats.immutable.MCTSStatsImmutableImpl
import com.github.robfitzgerald.dabtree.local.mctsstats.implicits._

class MCTSStatsMutableImplTests extends DefaultTest {
  "MCTSStatsMutableImpl" when {
    "empty[Double]()" should {
      "correctly show an empty object" in {
        val (low, high) = (0D, 1000D)
        val empty: MCTSStatsMutableImpl[Double] = MCTSStatsMutableImpl.empty[Double](Minimize(low, high))
        empty.min should equal (high)               // zero value for min operations
        empty.max should equal (low)                // zero value for max operations
        empty.mean should equal ((high + low) / 2)  // default value is midpoint
        empty.variance should equal (0D)
        empty.standardDeviation should equal (0D)
        empty.observations should equal (0)
      }
    }
    "empty[BigDecimal]()" should {
      "correctly show an empty object" in {
        val (low, high) = (BigDecimal(0), BigDecimal(1000))
        val empty: MCTSStatsMutableImpl[BigDecimal] = MCTSStatsMutableImpl.empty[BigDecimal](Minimize(low, high))
        empty.min should equal (high)               // zero value for min operations
        empty.max should equal (low)                // zero value for max operations
        empty.mean should equal ((high + low) / 2)  // default value is midpoint
        empty.variance should equal (BigDecimal(0))
        empty.standardDeviation should equal (BigDecimal(0))
        empty.observations should equal (0)
      }
    }
    "empty[Double]().add(5.0D)" should {
      "correctly reflect one observation" in {
        val five: MCTSStatsMutableImpl[Double] = MCTSStatsMutableImpl.empty[Double](Minimize(0D, 1000D)).update(5.0D)
        five.min should equal (5D)
        five.max should equal (5D)
        five.mean should equal (5D)
        five.variance should equal (0D)
        five.standardDeviation should equal (0D)
        five.observations should equal (1)
      }
    }
    "empty[Double]().add(2.5D).add(7.5D)" should {
      "correctly reflect two observations" in {
        val twoObservations: MCTSStatsMutableImpl[Double] = MCTSStatsMutableImpl.empty[Double](Minimize(0D, 1000D))
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
    ".toMutable" should {
      "not change the observed values" in {
        val twoObservations: MCTSStatsMutableImpl[Double] = MCTSStatsMutableImpl.empty[Double](Minimize(0D, 1000D)).update(2.5D).update(7.5D)
        val asImmutable: MCTSStatsImmutableImpl[Double] = twoObservations.toImmutable
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
