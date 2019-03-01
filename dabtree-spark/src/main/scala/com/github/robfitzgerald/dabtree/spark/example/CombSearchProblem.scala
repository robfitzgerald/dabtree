package com.github.robfitzgerald.dabtree.spark.example

import scala.util.Random

import com.github.robfitzgerald.dabtree.common.example.DabTreeCombSearchFunctions
import com.github.robfitzgerald.dabtree.spark.sampler.pedrosorei.Payload
import com.github.robfitzgerald.dabtree.spark.RankingDoublePrecision

trait CombSearchProblem {
  type State = Vector[Double]
  type Action = Double
  type Value = Double

  // problem description
  def minValue: Value
  def maxValue: Value
  def numChildren: Int
  def problemSize: Int
  def rankingPolicy: Payload[State, Action, Value] => Double
  def pStarPromotion: Double

  // algorithm parameters
  def activatedPayloadLimit: Int
  def totalPayloadCapacity: Int

  def possibleValues: Vector[Double] = Vector(0.00, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 1.00)

  final def vectorSize: Int = realSolution.size
  final def expandLimit: Int = if (vectorSize <= 6) vectorSize / 2 else vectorSize - 3

  val random: Random = new Random(System.currentTimeMillis())

  /**
    * The Solution that this combinatorial search instance is looking for
    */
  val realSolution: State = {
    for {
      i <- 1 to problemSize
      n = possibleValues(random.nextInt(possibleValues.size))
    } yield n
  }.toVector

  val dabTreeCombSearchFunctions = DabTreeCombSearchFunctions(
    realSolution,
    possibleValues,
    numChildren,
    expandLimit,
    new scala.util.Random(random.nextLong)
  )

  def startFrontier: List[(State, Option[Action])] = List((Vector.empty[Double], Option.empty[Action]))
}

object CombSearchProblem extends Serializable {

  def TreeDepthRankingPolicy(problemSize: Int): Payload[Vector[Double], Double, Double] => Double =
    (payload: Payload[Vector[Double], Double, Double]) => {
      val state: Vector[Double] = payload._1.state
      state.size.toDouble / problemSize
    }

  def CostBoundAndTreeDepthPolicy(problemSize: Int): Payload[Vector[Double], Double, Double] => Double = {
    val costLowerBounded = RankingDoublePrecision.CostLowerBoundedRanking[Vector[Double], Double]
    val treeDepth = TreeDepthRankingPolicy(problemSize)

    payload: Payload[Vector[Double], Double, Double] => {
      costLowerBounded(payload) + treeDepth(payload)
    }
  }

  def CostBoundTimesTreeDepthPolicy(problemSize: Int): Payload[Vector[Double], Double, Double] => Double = {
    val costLowerBounded = RankingDoublePrecision.CostLowerBoundedRanking[Vector[Double], Double]
    val treeDepth = TreeDepthRankingPolicy(problemSize)

    payload: Payload[Vector[Double], Double, Double] => {
      costLowerBounded(payload) * treeDepth(payload)
    }
  }
}