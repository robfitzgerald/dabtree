package com.github.robfitzgerald.banditsearch.mctsstats.immutable

import com.github.robfitzgerald.banditsearch.Objective
import com.github.robfitzgerald.banditsearch.mctsstats.MCTSStats
import com.github.robfitzgerald.banditsearch.mctsstats.mutable.MCTSStatsMutableImpl
import spire.implicits._
import spire.math._

/**
  * a generic representation of standard statistics
  * @param min the minimum-valued sample
  * @param max the maximum-valued sample
  * @param mean the mean value computed as a running average
  * @param varianceAccumulator the numerator of the assignment ```variance = (varianceNumerator / visits)```
  * @param observations count of samples observed
  */
case class MCTSStatsImmutableImpl [V : Numeric] (
  min: V,
  max: V,
  mean: V,
  varianceAccumulator: V,
  observations: Int = 0
) {
  def toMutable: MCTSStatsMutableImpl[V] = new MCTSStatsMutableImpl[V](min, max, mean, varianceAccumulator, observations)
}

object MCTSStatsImmutableImpl extends MCTSStatsImmutableTypeclass {
  def empty[V : Numeric](objective: Objective[V]): MCTSStatsImmutableImpl[V] = {
    objective match {
      case Objective.Minimize(optimalBounds, badBounds) =>
        val midpoint = (badBounds + optimalBounds) / 2
        MCTSStatsImmutableImpl(badBounds,optimalBounds,midpoint,0)
      case Objective.Maximize(optimalBounds, badBounds) =>
        val midpoint = (badBounds + optimalBounds) / 2
        MCTSStatsImmutableImpl(optimalBounds,badBounds,midpoint,0)
    }
  }
}

trait MCTSStatsImmutableTypeclass {
  implicit def MCTSStatsImmutableOps[V : Numeric]: MCTSStats[MCTSStatsImmutableImpl[V], V] =
    new MCTSStats[MCTSStatsImmutableImpl[V], V] {

      def update(a: MCTSStatsImmutableImpl[V], o: V): MCTSStatsImmutableImpl[V] = {
        val nextMin = if (a.observations == 0) o else MCTSStats.min(o, a.min)
        val nextMax = if (a.observations == 0) o else MCTSStats.max(o, a.max)
        val nextCount = a.observations + 1
        val nextMean: V = MCTSStats.runningMean(o, a.mean, nextCount)
        val nextVarianceAccumulator: V = MCTSStats.runningVariance(o, a.varianceAccumulator, a.mean, nextMean)
        a.copy (
          min = nextMin,
          max = nextMax,
          mean = nextMean,
          varianceAccumulator = nextVarianceAccumulator,
          observations = nextCount
        )
      }

      def min(a: MCTSStatsImmutableImpl[V]): V = a.min

      def max(a: MCTSStatsImmutableImpl[V]): V = a.max

      def mean(a: MCTSStatsImmutableImpl[V]): V = a.mean

      def variance(a: MCTSStatsImmutableImpl[V]): V = if (a.observations == 0) 0 else a.varianceAccumulator / a.observations

      def standardDeviation(a: MCTSStatsImmutableImpl[V]): V = if (a.observations == 0) 0 else sqrt(variance(a))

      def observations(a: MCTSStatsImmutableImpl[V]): Int = a.observations
    }
}

