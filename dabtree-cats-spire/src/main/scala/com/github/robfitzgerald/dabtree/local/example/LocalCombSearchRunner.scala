package com.github.robfitzgerald.dabtree.local.example

import com.github.robfitzgerald.dabtree.local.Objective
import com.github.robfitzgerald.dabtree.local.searchcontext.GenericPedrosoReiCollect.CollectResult
import com.github.robfitzgerald.dabtree.local.searchcontext.local.LocalSyncSearch
import spire.implicits._

trait LocalCombSearchRunner extends CombSearchProblem {

  final val localSyncSearch: LocalSyncSearch[State, Action, Value] =
    new LocalSyncSearch[State, Action, Value](
      simulate = this.dabTreeCombSearchFunctions.simulate,
      evaluate = this.dabTreeCombSearchFunctions.evaluate,
      objective = Objective.Minimize(minValue, maxValue),
      generateChildren = this.dabTreeCombSearchFunctions.generateChildren,
      rankingPolicy = this.rankingPolicy,
      allowChildExpansion = this.dabTreeCombSearchFunctions.allowChildExpansion,
      activatedPayloadLimit = this.activatedPayloadLimit,
      totalPayloadCapacity = this.totalPayloadCapacity,
      startFrontier = this.startFrontier,
      pStarPromotion = this.pStarPromotion
    )

  final def runSearch(iterationsMax: Int, durationMax: Long, samplesPerIteration: Int): Option[CollectResult[State, Value]] = {
    localSyncSearch.run(iterationsMax, durationMax, samplesPerIteration)
  }
}
