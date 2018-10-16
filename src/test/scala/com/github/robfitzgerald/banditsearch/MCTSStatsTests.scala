package com.github.robfitzgerald.banditsearch

import com.github.robfitzgerald.DefaultTest

class MCTSStatsTests extends DefaultTest {
  "MCTSStats" when {
    "empty[Double]()" should {
      "correctly show an empty object" in {
        val empty: MCTSStats[Double] = MCTSStats.empty[Double]()
        empty.min should equal (0D)
        empty.max should equal (0D)
        empty.mean should equal (0D)
        empty.median should equal (0D)
        empty.variance should equal (0D)
        empty.standardDeviation should equal (0D)
        empty.observations should equal (0)
      }
    }
    "empty[BigDecimal]()" should {
      "correctly show an empty object" in {
        val empty: MCTSStats[BigDecimal] = MCTSStats.empty[BigDecimal]()
        empty.min should equal (BigDecimal(0))
        empty.max should equal (BigDecimal(0))
        empty.mean should equal (BigDecimal(0))
        empty.median should equal (BigDecimal(0))
        empty.variance should equal (BigDecimal(0))
        empty.standardDeviation should equal (BigDecimal(0))
        empty.observations should equal (0)
      }
    }
    "empty[Double]().add(5.0D)" should {
      "correctly reflect one observation" in {
        val five: MCTSStats[Double] = MCTSStats.empty[Double]().add(5.0D)
        five.min should equal (5D)
        five.max should equal (5D)
        five.mean should equal (5D)
        five.median should equal (5D)
        five.variance should equal (0D)
        five.standardDeviation should equal (0D)
        five.observations should equal (1)
      }
    }
    "empty[Double]().add(2.5D).add(7.5D)" should {
      "correctly reflect two observations" in {
        val twoObservations: MCTSStats[Double] = MCTSStats.empty[Double]().add(2.5D).add(7.5D)
        val expectedVariance: Double = (math.pow(2.5-5, 2) + math.pow(7.5-5, 2)) / 2
        twoObservations.min should equal (2.5D)
        twoObservations.max should equal (7.5D)
        twoObservations.mean should equal (5D)
        twoObservations.median should equal (5D)
        twoObservations.variance should equal (expectedVariance)
        twoObservations.standardDeviation should equal (math.sqrt(expectedVariance))
        twoObservations.observations should equal (2)
      }
    }
  }
}
