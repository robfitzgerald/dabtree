package com.github.robfitzgerald.banditsearch.mctsstats.mutable

import com.github.robfitzgerald.banditsearch.mctsstats.MCTSStats
import spire.algebra.Trig
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
class MCTSStatsMutableImpl [V : Fractional] (
  var min: V,
  var max: V,
  var mean: V,
  var varianceAccumulator: V,
  var observations: Int = 0
)

object MCTSStatsMutableImpl {
  def empty[V : Fractional](): MCTSStatsMutableImpl[V] = new MCTSStatsMutableImpl[V](0, 0, 0, 0)

  implicit def MCTSStatsImmutableOps[V : Fractional](implicit ev: Trig[V]): MCTSStats[MCTSStatsMutableImpl[V], V] =
    new MCTSStats[MCTSStatsMutableImpl[V], V] {

      def update(a: MCTSStatsMutableImpl[V], o: V): MCTSStatsMutableImpl[V] = {
        val nextCount = a.observations + 1
        val nextMean: V = MCTSStats.runningMean(o, a.mean, nextCount)
        a.min = MCTSStats.min(o, a.min)
        a.max = MCTSStats.max(o, a.max)
        a.mean = nextMean
        a.varianceAccumulator = MCTSStats.runningVariance(o, a.varianceAccumulator, a.mean, nextMean)
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
