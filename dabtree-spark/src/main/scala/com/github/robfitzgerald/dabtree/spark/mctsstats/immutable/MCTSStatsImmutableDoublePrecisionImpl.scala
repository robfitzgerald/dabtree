package com.github.robfitzgerald.dabtree.spark.mctsstats.immutable

import com.github.robfitzgerald.dabtree.spark.mctsstats.HasStats
import com.github.robfitzgerald.dabtree.spark.mctsstats.mutable.MCTSStatsMutableDoublePrecisionImpl
import com.github.robfitzgerald.dabtree.spark.objective.{Objective, ObjectiveDoublePrecision}

/**
  * a generic representation of standard statistics
  * @param min the minimum-valued sample
  * @param max the maximum-valued sample
  * @param mean the mean value computed as a running average
  * @param varianceAccumulator the numerator of the assignment ```variance = (varianceNumerator / visits)```
  * @param observations count of samples observed
  */
case class MCTSStatsImmutableDoublePrecisionImpl (
  min: Double,
  max: Double,
  mean: Double,
  varianceAccumulator: Double = 0.0,
  observations: Long = 0
) extends HasStats {
  def toMutable: MCTSStatsMutableDoublePrecisionImpl = new MCTSStatsMutableDoublePrecisionImpl(min, max, mean, varianceAccumulator, observations)

  override def toString: String = f"min $min mean $mean max $max obs $observations"

  def min(o: Double, min: Double): Double = if (o < min) o else min
  def max(o: Double, max: Double): Double = if (o > max) o else max
  def median(min: Double, max: Double): Double = (min + max) / 2
  def variance: Double = if (observations < 2) 0 else varianceAccumulator / observations
  def standardDeviation: Double = if (observations == 0) 0 else math.sqrt(variance)
  
  def update(o: Double): MCTSStatsImmutableDoublePrecisionImpl = {
    val nextMin = if (this.observations == 0) o else this.min(o, this.min)
    val nextMax = if (this.observations == 0) o else this.max(o, this.max)
    val nextCount = this.observations + 1
    val nextMean: Double = MCTSStatsImmutableDoublePrecisionImpl.runningMean(o, this.mean, nextCount)
    val nextVarianceAccumulator: Double = MCTSStatsImmutableDoublePrecisionImpl.runningVariance(o, this.varianceAccumulator, this.mean, nextMean)
    this.copy (
      min = nextMin,
      max = nextMax,
      mean = nextMean,
      varianceAccumulator = nextVarianceAccumulator,
      observations = nextCount
    )
  }
}

object MCTSStatsImmutableDoublePrecisionImpl {
  def empty(objective: Objective[Double]): MCTSStatsImmutableDoublePrecisionImpl = {
    objective match {
      case ObjectiveDoublePrecision.Minimize(optimalBounds, badBounds) =>
        val midpoint: Double = (badBounds + optimalBounds) / 2
        MCTSStatsImmutableDoublePrecisionImpl(badBounds, optimalBounds, midpoint)
      case ObjectiveDoublePrecision.Maximize(optimalBounds, badBounds) =>
        val midpoint = (badBounds + optimalBounds) / 2
        MCTSStatsImmutableDoublePrecisionImpl(optimalBounds, badBounds, midpoint)
    }
  }

  def runningMean(o: Double, mean: Double, nextCount: Long): Double = mean + ((o - mean) / nextCount)
  def runningVariance(o: Double, vAcc: Double, mean: Double, nextMean: Double): Double = vAcc + ((o - mean) * (o - nextMean))
}

