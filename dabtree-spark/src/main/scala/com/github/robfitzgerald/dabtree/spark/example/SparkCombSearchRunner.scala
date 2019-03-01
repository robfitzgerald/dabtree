package com.github.robfitzgerald.dabtree.spark.example

import org.apache.spark.SparkContext

import com.github.robfitzgerald.dabtree.spark.objective.Objective
import com.github.robfitzgerald.dabtree.spark.objective._
import com.github.robfitzgerald.dabtree.spark.searchcontext.DoublePrecisionCollect.CollectResult
import com.github.robfitzgerald.dabtree.spark.searchcontext.SparkAsyncSearch

trait SparkCombSearchRunner extends CombSearchProblem {

  def sparkContext: SparkContext

  def parallelism: Int

  final val sparkAsyncSearch: SparkAsyncSearch[State, Action] =
    new SparkAsyncSearch[State, Action](
      sparkContext,
      parallelism,
      fns = dabTreeCombSearchFunctions,
      objective = ObjectiveDoublePrecision.Minimize(minValue, maxValue),
      rankingPolicy = this.rankingPolicy,
      activatedPayloadLimit = this.activatedPayloadLimit,
      totalPayloadCapacity = this.totalPayloadCapacity,
      startFrontier = this.startFrontier,
      pStarPromotion = this.pStarPromotion
    )

  final def runSearch(iterationsMax: Int, durationMax: Long, samplesPerIteration: Int): Option[CollectResult[State]] = {
    sparkAsyncSearch.run(iterationsMax, durationMax, samplesPerIteration)
  }
}
