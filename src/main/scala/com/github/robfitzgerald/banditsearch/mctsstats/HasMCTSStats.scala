package com.github.robfitzgerald.banditsearch.mctsstats

trait HasMCTSStats[V] {
  def min: V

  def max: V

  def mean: V

  def variance: V

  def standardDeviation: V

  def observations: Int
}
