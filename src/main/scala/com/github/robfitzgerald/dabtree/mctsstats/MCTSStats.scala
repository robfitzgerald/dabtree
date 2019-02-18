package com.github.robfitzgerald.dabtree.mctsstats

import com.github.robfitzgerald.dabtree.Objective
import com.github.robfitzgerald.dabtree.mctsstats.immutable.MCTSStatsImmutableImpl
import spire.algebra.Order
import spire.math.Numeric
import spire.implicits._


trait MCTSStats[A, V] {
  def update(a: A, observation: V): A

  def min(a: A): V

  def max(a: A): V

  def mean(a: A): V

  def variance(a: A): V

  def standardDeviation(a: A): V

  def observations(a: A): Int
}

object MCTSStats extends MCTSStatsTypeclass {

  def apply[V: Numeric](objective: Objective[V]): MCTSStatsImmutableImpl[V] = MCTSStatsImmutableImpl.empty[V](objective)

  def min[V: Order](o: V, min: V): V = if (o < min) o else min

  def max[V: Order](o: V, max: V): V = if (o > max) o else max

  def median[V: Numeric](min: V, max: V): V = (min + max) / 2

  def runningMean[V: Numeric](o: V, mean: V = 0, nextCount: Int = 1): V = mean + ((o - mean) / nextCount)

  def runningVariance[V: Numeric](o: V, vAcc: V, mean: V, nextMean: V): V = vAcc + ((o - mean) * (o - nextMean))
}

trait MCTSStatsTypeclass {

  implicit class MCTSStatsOps[A, V](a: A)(implicit ev: MCTSStats[A, V]) {
    def update(observation: V): A = ev.update(a, observation)

    def min: V = ev.min(a)

    def max: V = ev.max(a)

    def mean: V = ev.mean(a)

    def variance: V = ev.variance(a)

    def standardDeviation: V = ev.standardDeviation(a)

    def observations: Int = ev.observations(a)
  }

}