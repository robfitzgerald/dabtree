package com.github.robfitzgerald.dabtree.spark.objective

object ObjectiveDoublePrecision {
  case class Minimize(optimalBounds: Double, badBounds: Double) extends Objective[Double] {
    require(optimalBounds < badBounds, s"Minimize objective should have a 'optimalBounds' < 'badBounds' (since min is optimal), but instead $optimalBounds >= $badBounds.")

    override def zero: Double = badBounds

    override def optimal(a: Double, b: Double): Double = if (isMoreOptimalThan(a,b)) a else b

    override def bestSimulation(min: Double, max: Double): Double = min

    override def isMoreOptimalThan(a: Double, b: Double): Boolean = a < b
  }

  case class Maximize(badBounds: Double, optimalBounds: Double) extends Objective[Double] {
    require(optimalBounds >  badBounds, s"Maximize objective should have a 'optimalBounds' > 'badBounds' (since max is optimal), but instead $optimalBounds <= $badBounds.")

    override def zero: Double = badBounds

    override def optimal(a: Double, b: Double): Double = if (isMoreOptimalThan(a,b)) a else b

    override def bestSimulation(min: Double, max: Double): Double = max

    override def isMoreOptimalThan(a: Double, b: Double): Boolean = a > b
  }
}
