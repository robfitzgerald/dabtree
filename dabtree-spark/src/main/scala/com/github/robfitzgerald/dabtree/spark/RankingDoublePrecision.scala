package com.github.robfitzgerald.dabtree.spark

import com.github.robfitzgerald.dabtree.spark.sampler.pedrosorei.Payload

object RankingDoublePrecision {

  /**
    * a default ranking is not bad or good
    */
  private[RankingDoublePrecision] val DefaultRanking = 0.5D

  /**
    * a generic ranking function which ranks by reward value only
    * @tparam State generic state type
    * @tparam Action generic action type
    * @return an ordering rank, ascending
    */
  def RewardRanking[State, Action]: Payload[State, Action, Double] => Double =
    (payload: Payload[State, Action, Double]) => {
      payload._1.reward
    }

  /**
    * each partial solution has a cost, which is between the min and max globally-observed costs. this function
    * creates a ranking based on these values.
    * @tparam State generic state type
    * @tparam Action generic action type
    * @return an ordering rank, ascending
    */
  def CostLowerBoundedRanking[State, Action]: Payload[State, Action, Double] => Double =
    (payload: Payload[State, Action, Double]) => {
      val (banditParent, globalsOption, _) = payload

      {
        for {
          costBound <- banditParent.costBound
          globals <- globalsOption
        } yield {
          if (banditParent.mctsStats.observations < 2) DefaultRanking // at visits == 0 or 1, we don't have guarantees on global coefficient values
          else {
            val numer: Double = costBound - globals.state.gBest
            val denom: Double = globals.state.gWorst - globals.state.gBest

            1 - (numer / denom)
          }
        }
      } match {
        case None => DefaultRanking
        case Some(ranking) => ranking
      }
    }

  def LowerBoundedAndRewardRanking[State, Action]: Payload[State, Action, Double] => Double = {
    val costLowerBounded = CostLowerBoundedRanking[State,Action]
    val generic = RewardRanking[State,Action]
    payload: Payload[State, Action, Double] => {
      costLowerBounded(payload) + generic(payload)
    }
  }
}
