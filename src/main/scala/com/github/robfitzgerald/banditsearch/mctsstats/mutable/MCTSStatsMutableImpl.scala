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
class MCTSStatsMutableImpl [F[_], V : Numeric] (
  var min: V,
  var max: V,
  var mean: V,
  var varianceAccumulator: V,
  var observations: Int = 0
)

object MCTSStatsMutableImpl {
  def empty[F[_], V : Numeric](): MCTSStatsMutableImpl[F,V] = new MCTSStatsMutableImpl[F, V](0, 0, 0, 0)

  implicit def MCTSStatsImmutableOps[F[_], V : Numeric](implicit ev: Trig[V]): MCTSStats[F, MCTSStatsMutableImpl[F, V], V] =
    new MCTSStats[F, MCTSStatsMutableImpl[F, V], V] {

      def update(a: MCTSStatsMutableImpl[F, V], o: V): MCTSStatsMutableImpl[F, V] = {
        val nextCount = a.observations + 1
        val nextMean: V = MCTSStats.runningMean(o, a.mean, nextCount)
        a.min = MCTSStats.min(o, a.min)
        a.max = MCTSStats.max(o, a.max)
        a.mean = nextMean
        a.varianceAccumulator = MCTSStats.runningVariance(o, a.varianceAccumulator, a.mean, nextMean)
        a.observations = nextCount
        a
      }

      def min(a: MCTSStatsMutableImpl[F, V]): V = a.min

      def max(a: MCTSStatsMutableImpl[F, V]): V = a.max

      def mean(a: MCTSStatsMutableImpl[F, V]): V = a.mean

      def variance(a: MCTSStatsMutableImpl[F, V]): V = if (a.observations == 0) 0 else a.varianceAccumulator / a.observations

      def standardDeviation(a: MCTSStatsMutableImpl[F, V]): V = if (a.observations == 0) 0 else sqrt(variance(a))

      def observations(a: MCTSStatsMutableImpl[F, V]): Int = a.observations
    }
}
