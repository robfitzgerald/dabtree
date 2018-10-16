package com.github.robfitzgerald.banditsearch

import spire.math._
import spire.implicits._

/**
  * a generic representation of standard statistics
  * @param min the minimum-valued sample
  * @param max the maximum-valued sample
  * @param mean the mean value computed as a running average
  * @param varianceNumerator the numerator of the assignment ```variance = (varianceNumerator / visits)```
  * @param observations count of samples observed
  */
case class MCTSStats [V : Fractional] (
  min: V,
  max: V,
  mean: V,
  varianceNumerator: V,
  observations: Int = 0
) {
  lazy val median: V = (min + max) / 2
  lazy val variance: V = if (observations == 0) 0 else varianceNumerator / observations
  lazy val standardDeviation: V = if (observations == 0) 0 else sqrt(variance)

  def add(observation: V): MCTSStats[V] = {
    if (observations == 0) {
      MCTSStats[V](min = observation, max = observation, mean = observation, 0, observations = 1)
    } else {
      val nextCount: Int = observations + 1
      val nextMin: V = if (observation < min) observation else min
      val nextMax: V = if (observation > max) observation else max
      val nextMean: V = mean + ((observation - mean) / nextCount)
      val nextRunningVarianceValue: V = varianceNumerator + ((observation - mean) * (observation - nextMean))

      MCTSStats(nextMin, nextMax, nextMean, nextRunningVarianceValue, nextCount)
    }
  }
}

object MCTSStats {
  def empty[V : Fractional](): MCTSStats[V] = MCTSStats[V](0, 0, 0, 0)
}


