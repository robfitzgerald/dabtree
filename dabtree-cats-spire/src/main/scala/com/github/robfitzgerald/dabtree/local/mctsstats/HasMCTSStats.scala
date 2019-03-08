package com.github.robfitzgerald.dabtree.local.mctsstats

trait HasMCTSStats[V] {
  def min: V

  def max: V

  def mean: V

  def variance: V

  def standardDeviation: V

  def observations: Int
}
