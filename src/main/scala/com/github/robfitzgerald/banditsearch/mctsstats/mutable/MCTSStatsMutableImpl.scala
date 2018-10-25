package com.github.robfitzgerald.banditsearch.mctsstats.mutable

import com.github.robfitzgerald.banditsearch.Objective
import com.github.robfitzgerald.banditsearch.mctsstats.MCTSStats
import com.github.robfitzgerald.banditsearch.mctsstats.immutable.MCTSStatsImmutableImpl
import spire.implicits._
import spire.math._

/**
  * a generic representation of standard statistics which performs its update in-place
  * @param min the minimum-valued sample
  * @param max the maximum-valued sample
  * @param mean the mean value computed as a running average
  * @param varianceAccumulator the numerator of the assignment ```variance = (varianceNumerator / visits)```
  * @param observations count of samples observed
  */
class MCTSStatsMutableImpl [V : Numeric] (
  var min: V,
  var max: V,
  var mean: V,
  var varianceAccumulator: V,
  var observations: Int = 0
) {
  def toImmutable: MCTSStatsImmutableImpl[V] = MCTSStatsImmutableImpl(min, max, mean, varianceAccumulator, observations)
}

object MCTSStatsMutableImpl extends MCTSStatsMutableTypeclass {
  def empty[V : Numeric](objective: Objective[V]): MCTSStatsMutableImpl[V] = {
    objective match {
      case Objective.Minimize(optimalBounds, badBounds) =>
        val midpoint = (badBounds + optimalBounds) / 2
        new MCTSStatsMutableImpl(badBounds,optimalBounds,midpoint,0)
      case Objective.Maximize(optimalBounds, badBounds) =>
        val midpoint = (badBounds + optimalBounds) / 2
        new MCTSStatsMutableImpl(optimalBounds,badBounds,midpoint,0)
    }
  }
}

trait MCTSStatsMutableTypeclass {
  implicit def MCTSStatsMutableOps[V : Numeric]: MCTSStats[MCTSStatsMutableImpl[V], V] =
    new MCTSStats[MCTSStatsMutableImpl[V], V] {

      def update(a: MCTSStatsMutableImpl[V], o: V): MCTSStatsMutableImpl[V] = {
        val nextMin = if (a.observations == 0) o else MCTSStats.min(o, a.min)
        val nextMax = if (a.observations == 0) o else MCTSStats.max(o, a.max)
        val nextCount = a.observations + 1
        val nextMean: V = MCTSStats.runningMean(o, a.mean, nextCount)
        val nextVarianceAccumulator: V = MCTSStats.runningVariance(o, a.varianceAccumulator, a.mean, nextMean)
        a.min = nextMin
        a.max = nextMax
        a.mean = nextMean
        a.varianceAccumulator = nextVarianceAccumulator
        a.observations = nextCount
        a
      }

      def min(a: MCTSStatsMutableImpl[V]): V = a.min

      def max(a: MCTSStatsMutableImpl[V]): V = a.max

      def mean(a: MCTSStatsMutableImpl[V]): V = a.mean

      def variance(a: MCTSStatsMutableImpl[V]): V = if (a.observations == 0) 0 else a.varianceAccumulator / a.observations

      def standardDeviation(a: MCTSStatsMutableImpl[V]): V = if (a.observations == 0) 0 else sqrt(variance(a))

      def observations(a: MCTSStatsMutableImpl[V]): Int = a.observations
    }
}