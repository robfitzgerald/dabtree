package com.github.robfitzgerald.banditsearch.mctsstats.immutable

import com.github.robfitzgerald.banditsearch.mctsstats.MCTSStats
import spire.algebra.Trig
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
case class MCTSStatsImmutableImpl [V : Fractional] (
  min: V,
  max: V,
  mean: V,
  varianceAccumulator: V,
  observations: Int = 0
)

object MCTSStatsImmutableImpl {
  def empty[V : Fractional](): MCTSStatsImmutableImpl[V] = MCTSStatsImmutableImpl[V](0, 0, 0, 0)

  implicit def MCTSStatsImmutableOps[V : Fractional](implicit ev: Trig[V]): MCTSStats[MCTSStatsImmutableImpl[V], V] =
    new MCTSStats[MCTSStatsImmutableImpl[V], V] {

      def update(a: MCTSStatsImmutableImpl[V], o: V): MCTSStatsImmutableImpl[V] = {
        val nextCount = a.observations + 1
        val nextMean: V = MCTSStats.runningMean(o, a.mean, nextCount)
        a.copy (
          MCTSStats.min(o, a.min),
          MCTSStats.max(o, a.max),
          nextMean,
          MCTSStats.runningVariance(o, a.varianceAccumulator, a.mean, nextMean),
          nextCount
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


