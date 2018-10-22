package com.github.robfitzgerald.banditsearch.reward

import com.github.robfitzgerald.banditsearch.Objective
import com.github.robfitzgerald.banditsearch.banditnode.HasMCTSStats
import spire.math._
import spire.implicits._
import spire.algebra._

object UCBPedrosoRei {
  def rewardFunction[V : Numeric : Trig](node: HasMCTSStats[V], gBest: V, gWorst: V, parentVisits: Int, objective: Objective[V], Cp: Double)(observation: V): Double = {
    if (node.mctsStats.observations == 0) 0D
    else {

      val exploitation: Double = {
        if (gWorst == gBest) 0D
        else {
          pedrosoReiXTerm(gBest, gWorst, objective.bestSimulation(node))
        }
      }

      val exploration = {
        if (Cp == 0) 0D
        else if (parentVisits == 0) 0D
        else if (node.mctsStats.observations == 0) Double.PositiveInfinity
        else if (gWorst == gBest) Double.PositiveInfinity
        else {

          val xbar = pedrosoReiXTerm(gBest, gWorst, node.mctsStats.mean)
          val E = Cp * math.sqrt(math.log(parentVisits) / node.mctsStats.observations)

          xbar * E
        }
      }

      exploitation + exploration
    }
  }

  /**
    * the "x" term in pedroso rei's UCT function
    *
    * @param globalBestSimulation best simulation observed locally/globally
    * @param globalWorstSimulation worst simulation observed locally/globally
    * @param localSimulation either local best or local average
    * @return
    */
  def pedrosoReiXTerm[V : Trig](globalBestSimulation: V, globalWorstSimulation: V, localSimulation: V)(implicit t: Numeric[V]): Double = {
    val a: V = (globalWorstSimulation - localSimulation) / (globalWorstSimulation - globalBestSimulation)
    val numer: V = exp(a) - 1
    val denom: Double = e - 1
    if (denom != 0D) t.div(numer, t.fromDouble(denom)).toDouble else 0D
  }
}