package com.github.robfitzgerald.banditsearch.reward

import spire.math._
import spire.implicits._
import spire.algebra._

object UCBPedrosoRei {
  def rewardFunction[V : Numeric : Trig](gBest: V, gWorst: V, lBest: V, lAvg: V, pVisits: Int, cVisits: Int, Cp: Double): Double = {
    if (pVisits == 0) 0D
    else {

      val exploitation: Double = {
        if (gWorst == gBest) 0D
        else {
          pedrosoReiXTerm(gBest, gWorst, lBest)
        }
      }

      val exploration = {
        if (Cp == 0) 0D
        else if (pVisits == 0) 0D
        else if (cVisits == 0) Double.PositiveInfinity
        else if (gWorst == gBest) Double.PositiveInfinity
        else {

          val xbar = pedrosoReiXTerm(gBest, gWorst, lAvg)
          val E = Cp * math.sqrt(math.log(pVisits) / cVisits)

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
  def pedrosoReiXTerm[V : Numeric](globalBestSimulation: V, globalWorstSimulation: V, localSimulation: V)(implicit t: Trig[V]): Double = {
    val a: V = (globalWorstSimulation - localSimulation) / (globalWorstSimulation - globalBestSimulation)
    val numer: V = t.exp(a) - 1
    val denom: Double = (t.e - 1).toDouble
    if (denom != 0D) numer.toDouble / denom else 0D
  }
}