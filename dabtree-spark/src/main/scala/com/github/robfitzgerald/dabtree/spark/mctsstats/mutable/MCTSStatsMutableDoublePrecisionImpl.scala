package com.github.robfitzgerald.dabtree.spark.mctsstats.mutable

import com.github.robfitzgerald.dabtree.spark.mctsstats.HasStats
import com.github.robfitzgerald.dabtree.spark.mctsstats.immutable.MCTSStatsImmutableDoublePrecisionImpl
import com.github.robfitzgerald.dabtree.spark.objective.{Objective, ObjectiveDoublePrecision}

/**
  * a generic representation of standard statistics which performs its update in-place
  * @param min the minimum-valued sample
  * @param max the maximum-valued sample
  * @param mean the mean value computed as a running average
  * @param varianceAccumulator the numerator of the assignment ```variance = (varianceNumerator / visits)```
  * @param observations count of samples observed
  */
class MCTSStatsMutableDoublePrecisionImpl (
  var min: Double,
  var max: Double,
  var mean: Double,
  var varianceAccumulator: Double = 0.0,
  var observations: Long = 0
) extends HasStats {
  def toImmutable: MCTSStatsImmutableDoublePrecisionImpl = MCTSStatsImmutableDoublePrecisionImpl(min, max, mean, varianceAccumulator, observations)

  def min(o: Double, min: Double): Double = if (o < min) o else min
  def max(o: Double, max: Double): Double = if (o > max) o else max
  def median(min: Double, max: Double): Double = (min + max) / 2
  def variance: Double = if (observations < 2) 0 else varianceAccumulator / observations
  def standardDeviation: Double = if (observations == 0) 0 else math.sqrt(variance)

  def update(o: Double): Unit = {
    val nextMin = if (this.observations == 0) o else this.min(o, this.min)
    val nextMax = if (this.observations == 0) o else this.max(o, this.max)
    val nextCount = this.observations + 1
    val nextMean: Double = MCTSStatsMutableDoublePrecisionImpl.runningMean(o, this.mean, nextCount)
    val nextVarianceAccumulator: Double = MCTSStatsMutableDoublePrecisionImpl.runningVariance(o, this.varianceAccumulator, this.mean, nextMean)
    this.min = nextMin
    this.max = nextMax
    this.mean = nextMean
    this.varianceAccumulator = nextVarianceAccumulator
    this.observations = nextCount
    ()
  }
}

object MCTSStatsMutableDoublePrecisionImpl {
  def empty(objective: Objective[Double]): MCTSStatsMutableDoublePrecisionImpl = {
    objective match {
      case ObjectiveDoublePrecision.Minimize(optimalBounds, badBounds) =>
        val midpoint = (badBounds + optimalBounds) / 2
        new MCTSStatsMutableDoublePrecisionImpl(badBounds,optimalBounds,midpoint)
      case ObjectiveDoublePrecision.Maximize(optimalBounds, badBounds) =>
        val midpoint = (badBounds + optimalBounds) / 2
        new MCTSStatsMutableDoublePrecisionImpl(optimalBounds,badBounds,midpoint)
    }
  }

  def runningMean(o: Double, mean: Double, nextCount: Long): Double = mean + ((o - mean) / nextCount)
  def runningVariance(o: Double, vAcc: Double, mean: Double, nextMean: Double): Double = vAcc + ((o - mean) * (o - nextMean))
}