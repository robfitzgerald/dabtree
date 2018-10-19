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
case class MCTSStatsImmutableImpl [F[_], V : Numeric] (
  min: V,
  max: V,
  mean: V,
  varianceAccumulator: V,
  observations: Int = 0
)

object MCTSStatsImmutableImpl {
  def empty[F[_], V : Numeric](): MCTSStatsImmutableImpl[F, V] = MCTSStatsImmutableImpl[F, V](0,0,0,0)

  implicit def MCTSStatsImmutableOps[F[_], V : Numeric](implicit ev: Trig[V]): MCTSStats[F, MCTSStatsImmutableImpl[F, V], V] =
    new MCTSStats[F, MCTSStatsImmutableImpl[F, V], V] {

      def update(a: MCTSStatsImmutableImpl[F, V], o: V): MCTSStatsImmutableImpl[F, V] = {
        val nextCount = a.observations + 1
        val nextMean: V = MCTSStats.runningMean(o, a.mean, nextCount)
        a.copy (
          MCTSStats.min(o, a.min),
          MCTSStats.max(o, a.max),
          nextMean,
          MCTSStats.runningVariance[V](o, a.varianceAccumulator, a.mean, nextMean),
          nextCount
        )
      }

      def min(a: MCTSStatsImmutableImpl[F, V]): V = a.min

      def max(a: MCTSStatsImmutableImpl[F, V]): V = a.max

      def mean(a: MCTSStatsImmutableImpl[F, V]): V = a.mean

      def variance(a: MCTSStatsImmutableImpl[F, V]): V = if (a.observations == 0) 0 else a.varianceAccumulator / a.observations

      def standardDeviation(a: MCTSStatsImmutableImpl[F, V]): V = if (a.observations == 0) 0 else sqrt(variance(a))

      def observations(a: MCTSStatsImmutableImpl[F, V]): Int = a.observations
    }
}


