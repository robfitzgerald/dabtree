package com.github.robfitzgerald.dabtree.spark.searchcontext

import org.apache.spark.util.AccumulatorV2

import com.github.robfitzgerald.dabtree.common.SearchStats
import com.github.robfitzgerald.dabtree.spark.searchcontext.CancelledPayloadAccumulator.CancelledData


/**
  * allows for tracking all events related to Cancelled payloads in a Spark context
  * @param isEmpty whether this accumulator has any data
  * @param cancelledData the accumulator of Cancelled data
  */
class CancelledPayloadAccumulator(
  var isEmpty: Boolean = true,
  var cancelledData: CancelledData = CancelledData()
) extends AccumulatorV2[CancelledData, CancelledData] {

  def isZero: Boolean = isEmpty

  def copy(): AccumulatorV2[CancelledData, CancelledData] = new CancelledPayloadAccumulator(this.isEmpty, this.cancelledData)

  def reset(): Unit = {
    this.isEmpty = true
    this.cancelledData = CancelledData()
  }

  def add(v: CancelledData): Unit = {
    this.cancelledData = this.cancelledData.copy(
      searchStats = this.cancelledData.searchStats ++ v.searchStats,
      observations = this.cancelledData.observations + v.observations
    )
    this.isEmpty = false
  }

  def merge(other: AccumulatorV2[CancelledData, CancelledData]): Unit = {
    this.isEmpty = this.isZero || other.isZero
    this.cancelledData = this.cancelledData.copy(
      searchStats = this.cancelledData.searchStats ++ other.value.searchStats,
      observations = this.cancelledData.observations + other.value.observations
    )
  }

  def value: CancelledData = this.cancelledData
}

object CancelledPayloadAccumulator {
  case class CancelledData(
    searchStats: SearchStats = SearchStats(),
    observations: Long = 0L
  )
}