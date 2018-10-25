package com.github.robfitzgerald.banditsearch.example

import com.github.robfitzgerald.banditsearch.Objective
import com.github.robfitzgerald.banditsearch.pedrosorei.Payload
import com.github.robfitzgerald.banditsearch.searchcontext.GenericPedrosoReiCollect.CollectResult
import com.github.robfitzgerald.banditsearch.searchcontext.local.LocalSyncSearch
import spire.implicits._

trait CombSearch {
  type State = Vector[Double]
  type Action = Double
  type Value = Double

  // problem description
  def realSolution: State
  def minValue: Value = 0
  def maxValue: Value = math.sqrt(realSolution.size.toDouble)

  // algorithm parameters
  def activatedPayloadLimit: Int
  def totalPayloadCapacity: Int

  def possibleValues: Vector[Double] = Vector(0.00, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 1.00)

  //
  final def vectorSize: Int = realSolution.size
  final def expandLimit: Int = if (vectorSize <= 6) vectorSize / 2 else vectorSize - 3

  val random = spire.random.rng.Cmwc5()

  // randomly fill our vector
  final val simulate: State => State = (state: State) => {
    state ++ {
      for {
        _ <- 1 to (vectorSize - state.size)
        n = possibleValues(random.nextInt(possibleValues.size))
      } yield {
        n
      }
    }
  }

  // evaluate as the euclidian distance, which we wish to minimize
  final val evaluate: State => Value = (state: State) => {
    val sum: Value = state.
      zip(realSolution).
      map { case (a, b) => math.pow(a - b, 2) }.
      fold(0D) { (sum, n) => sum + n }
    math.sqrt(sum)
  }

  // generate values in the range [0, 1] stepping by 0.05 (21 values)
  final val generateChildren: State => Array[(State, Option[Action])] = (state: State) => {
    {
      for {
        n <- possibleValues
      } yield (state :+ n, Some(n))
    }.toArray
  }

  // rank by reward
  final val rankingPolicy: Payload[State, Action, Value] => Double = (payload: Payload[State, Action, Value]) => {
    payload._1.reward
  }

  // allow up to the user-provided expand limit
  final val allowChildExpansion: State => Boolean = (state: State) => {
    state.size < expandLimit
  }

  def startFrontier: List[(State, Option[Action])] = List((Vector.empty[Double], Option.empty[Action]))

  final val localSyncSearch: LocalSyncSearch[State, Action, Value] =
    new LocalSyncSearch[State, Action, Value](
      simulate = this.simulate,
      evaluate = this.evaluate,
      objective = Objective.Minimize(minValue, maxValue),
      generateChildren = this.generateChildren,
      rankingPolicy = this.rankingPolicy,
      allowChildExpansion = this.allowChildExpansion,
      activatedPayloadLimit = this.activatedPayloadLimit,
      totalPayloadCapacity = this.totalPayloadCapacity,
      startFrontier = this.startFrontier
    )

  final def runSearch(iterationsMax: Int, durationMax: Long, samplesPerIteration: Int): Option[CollectResult[State, Value]] = {
    localSyncSearch.run(iterationsMax, durationMax, samplesPerIteration)
  }
}
