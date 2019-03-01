package com.github.robfitzgerald.dabtree.spark.searchcontext

import scala.annotation.tailrec

import org.apache.spark.SparkContext
import org.apache.spark.broadcast.Broadcast
import org.apache.spark.rdd.RDD
import cats.implicits._

import com.github.robfitzgerald.dabtree.common.banditnode.SearchState
import com.github.robfitzgerald.dabtree.common.DabTreeFunctionParameters
import com.github.robfitzgerald.dabtree.spark.banditnode.SparkBanditParent
import com.github.robfitzgerald.dabtree.spark.objective.Objective
import com.github.robfitzgerald.dabtree.spark.randomselection.RandomSelection
import com.github.robfitzgerald.dabtree.spark.sampler.SamplerDoublePrecision
import com.github.robfitzgerald.dabtree.spark.sampler.pedrosorei.{Payload, UCBPedrosoReiGlobalState, UCBPedrosoReiGlobals, UCBPedrosoReiMetaParameters}
import com.github.robfitzgerald.dabtree.spark.searchcontext.DoublePrecisionCollect.CollectResult

class SparkAsyncSearch[S, A](
  sparkContext               : SparkContext,
  parallelism                : Int,
  fns                        : DabTreeFunctionParameters[S, A, Double],
  objective                  : Objective[Double],
  rankingPolicy              : Payload[S, A, Double] => Double,
  activatedPayloadLimit      : Int,
  totalPayloadCapacity       : Int,
  startFrontier              : List[(S, Option[A])],
  synchronize                : Boolean = true,
  explorationCoefficient     : Int => Double = (_: Int) => math.sqrt(2),
  explorationUpdate          : Option[Double => Double] = None,
  expandObservationsThreshold: Int = 30,
  pStarPromotion             : Double = 0.5D,
  maxExpandPerIteration      : Int = 2
) {

  val partitions = 3

  def run(iterationsMax: Int, durationMax: Long, samplesPerIteration: Int): Option[CollectResult[S]] = {
    val stopTime: Long = System.currentTimeMillis() + durationMax

    val bCastFns = sparkContext.broadcast(fns)
    val bCastRandom = sparkContext.broadcast(RandomSelection.scalaRandom[S, A])
    val bCastObjective = sparkContext.broadcast(objective)

    val sampler: SamplerDoublePrecision[S, A] = SamplerDoublePrecision(bCastFns, bCastRandom, bCastObjective)

    val expandFunction: Payload[S, A, Double] => List[Payload[S, A, Double]] =
      SparkAsyncSearch.serializationSafeExpandFunction(
        expandObservationsThreshold,
        pStarPromotion,
        maxExpandPerIteration,
        objective,
        bCastFns
      )

    val rebalanceFunction: List[Payload[S, A, Double]] => List[Payload[S, A, Double]] =
      SparkAsyncSearch.serializationSafeRebalancingFunction(activatedPayloadLimit, totalPayloadCapacity, rankingPolicy)

    val bCastSamplesPerIteration = sparkContext.broadcast(samplesPerIteration)
    val bCastSampler = sparkContext.broadcast(sampler)
    val bCastExpandFunction = sparkContext.broadcast(expandFunction)
    val bCastRebalanceFunction = sparkContext.broadcast(rebalanceFunction)



    /**
      * runs each iteration of our algorithm, with break points in place to short-circuit for when we exceed our compute time
      *
      * @param frontier the group of payloads where we are concentrating our MCTS search
      * @param it       current iteration number
      * @return the final frontier.. and number of iterations
      */
    @tailrec
    def _run(frontier: RDD[Payload[S, A, Double]], it: Int = 1): (RDD[Payload[S, A, Double]], Int) = {

      if (it > iterationsMax || System.currentTimeMillis() > stopTime) (frontier, it - 1)
      else {

        ///////////////////////
        // 1 --- sample step //
        ///////////////////////
        val sampledFrontier: RDD[Payload[S, A, Double]] =
        frontier.
          map { case (parent, globals) =>
            if (parent.searchState == SearchState.Activated) {
               val updatedPayload: Payload[S, A, Double] = bCastSampler.value.run((parent, globals), bCastSamplesPerIteration.value).value
//                              print("^")

              updatedPayload
            } else {
              //                print("_")
              (parent, globals)
            }
          }
        //        print("\n")

        if (it > iterationsMax || System.currentTimeMillis() > stopTime) (sampledFrontier, it)
        else {

          ////////////////////////////
          // 2 --- synchronize step //
          ////////////////////////////
          val syncedFrontier: RDD[Payload[S, A, Double]] =
          if (!synchronize) sampledFrontier
          else SparkDoublePrecisionSynchronization.synchronize[S, A](sampledFrontier, bCastSampler, bCastObjective)


          if (it > iterationsMax || System.currentTimeMillis() > stopTime) (syncedFrontier, it)
          else {

            ///////////////////////////////////
            // 3 --- expand & rebalance step //
            ///////////////////////////////////
            val rebalancedFrontier: RDD[Payload[S, A, Double]] = syncedFrontier.mapPartitions { partitionIterator =>
              val expanded = partitionIterator.
                toList.
                flatMap { payload =>
                  if (payload._1.searchState != SearchState.Activated) {
                    List(payload)
                  } else {
                    bCastExpandFunction.value(payload)
                  }
                }
              val rebalanced = bCastRebalanceFunction.value(expanded)
              rebalanced.toIterator
            }

            _run(rebalancedFrontier, it + 1)
          }
        }
      }
    }

    // build starting payloads based on user-provided exploration coefficient function and objective
    val startFrontierPayloads: List[Payload[S, A, Double]] = {
      for {
        _ <- 0 until partitions
        ((state, action), index) <- startFrontier.zipWithIndex
      } yield {
        val newParent = SparkBanditParent.frontierPayload(
          state,
          action,
          Some(fns.evaluate(_)),
          fns.generateChildren,
          objective
        )
        val newGlobalState = UCBPedrosoReiGlobalState[S, A, Double](objective)
        val newGlobalMeta = UCBPedrosoReiMetaParameters(explorationCoefficient(index))
        (newParent, Some(UCBPedrosoReiGlobals[S, A, Double](newGlobalState, newGlobalMeta)))
      }
    }.toList

    val (searchResult: RDD[Payload[S, A, Double]], iterationsCount: Int) = _run(sparkContext.parallelize(startFrontierPayloads), parallelism)

    DoublePrecisionCollect.collectDoublePrecision(searchResult, objective, iterationsCount)
  }
}

object SparkAsyncSearch {

  def serializationSafeExpandFunction[S, A](
    expObsThresh: Int,
    rwdThresh: Double,
    maxExpand: Int,
    objective: Objective[Double],
    bCastFns: Broadcast[DabTreeFunctionParameters[S, A, Double]]
  ): Payload[S, A, Double] => List[Payload[S, A, Double]] = DoublePrecisionExpansion.expand[List, S, A](
    expObsThresh,
    rwdThresh,
    maxExpand,
    objective,
    bCastFns.value.allowChildExpansion,
    Some(bCastFns.value.evaluate),
    bCastFns.value.generateChildren
  )

  def serializationSafeRebalancingFunction[S, A](
    actLim: Int,
    pLim: Int,
    ranking: Payload[S, A, Double] => Double
  ): List[Payload[S, A, Double]] => List[Payload[S, A, Double]] = DoublePrecisionRebalancing.rebalance[List, S, A](
    actLim,
    pLim,
    ranking
  )
}
