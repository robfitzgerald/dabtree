package com.github.robfitzgerald.banditsearch.mctsstats.immutable

import spire.implicits._

import com.github.robfitzgerald.DefaultTest
import com.github.robfitzgerald.banditsearch.Objective.Minimize
import com.github.robfitzgerald.banditsearch.mctsstats.implicits._
import com.github.robfitzgerald.banditsearch.mctsstats.mutable.MCTSStatsMutableImpl

class MCTSStatsImmutableImplTests extends DefaultTest {
  "MCTSStatsImmutableImpl" when {
    "empty[Double]()" should {
      "correctly show an empty object" in {
        val empty: MCTSStatsImmutableImpl[Double] = MCTSStatsImmutableImpl.empty[Double](Minimize(0D, 1000D))
        empty.min should equal (0D)
        empty.max should equal (0D)
        empty.mean should equal (0D)
        empty.variance should equal (0D)
        empty.standardDeviation should equal (0D)
        empty.observations should equal (0)
      }
    }
    "empty[BigDecimal]()" should {
      "correctly show an empty object" in {
        val empty: MCTSStatsImmutableImpl[BigDecimal] = MCTSStatsImmutableImpl.empty[BigDecimal](Minimize(BigDecimal(0), BigDecimal(1000)))
        empty.min should equal (BigDecimal(0))
        empty.max should equal (BigDecimal(0))
        empty.mean should equal (BigDecimal(0))
        empty.variance should equal (BigDecimal(0))
        empty.standardDeviation should equal (BigDecimal(0))
        empty.observations should equal (0)
      }
    }
    "empty[Double]().add(5.0D)" should {
      "correctly reflect one observation" in {
        val five: MCTSStatsImmutableImpl[Double] = MCTSStatsImmutableImpl.empty[Double](Minimize(0D, 1000D)).update(5.0D)
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
        val twoObservations: MCTSStatsImmutableImpl[Double] = MCTSStatsImmutableImpl.empty[Double](Minimize(0D, 1000D)).update(2.5D).update(7.5D)
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
        val twoObservations: MCTSStatsImmutableImpl[Double] = MCTSStatsImmutableImpl.empty[Double](Minimize(0D, 1000D)).update(2.5D).update(7.5D)
        val asMutable: MCTSStatsMutableImpl[Double] = twoObservations.toMutable
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
