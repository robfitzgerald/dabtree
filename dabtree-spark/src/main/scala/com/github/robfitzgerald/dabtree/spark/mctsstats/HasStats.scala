package com.github.robfitzgerald.dabtree.spark.mctsstats

trait HasStats {
  def min: Double
  def max: Double
  def mean: Double
//  def median: Double
//  def runningMean(o: Double, mean: Double = 0, nextCount: Int = 1): Double
//  def runningVariance(o: Double, vAcc: Double, mean: Double, nextMean: Double): Double
  def observations: Int
}
