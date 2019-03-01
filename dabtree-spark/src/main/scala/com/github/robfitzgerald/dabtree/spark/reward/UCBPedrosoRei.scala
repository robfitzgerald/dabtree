package com.github.robfitzgerald.dabtree.spark.reward

object UCBPedrosoRei {

  def rewardFunction(gBest: Double, gWorst: Double, lBest: Double, lAvg: Double, pVisits: Int, cVisits: Int, Cp: Double): Double = {

    val exploitation: Double = {
      if (gWorst == gBest) 0D
      else {
        pedrosoReiXTerm(gBest, gWorst, lBest)
      }
    }

    val exploration = {
      if (Cp == 0) 0D
      else if (cVisits == 0) Double.PositiveInfinity
      else if (pVisits == 0) 0D
      else if (gWorst == gBest) Double.PositiveInfinity
      else {

        val xbar = pedrosoReiXTerm(gBest, gWorst, lAvg)
        val E = Cp * math.sqrt(math.log(pVisits) / cVisits)

        xbar * E
      }
    }

    exploitation + exploration

  }

  /**
    * the "x" term in pedroso rei's UCT function
    *
    * @param globalBestSimulation best simulation observed locally/globally
    * @param globalWorstSimulation worst simulation observed locally/globally
    * @param localSimulation either local best or local average
    * @return
    */
  def pedrosoReiXTerm(globalBestSimulation: Double, globalWorstSimulation: Double, localSimulation: Double): Double = {
    val a: Double = (globalWorstSimulation - localSimulation) / (globalWorstSimulation - globalBestSimulation)
    val numer: Double = math.exp(a) - 1.0
    val denom: Double = math.E - 1.0
    if (denom != 0D) numer / denom else 0D
  }
}