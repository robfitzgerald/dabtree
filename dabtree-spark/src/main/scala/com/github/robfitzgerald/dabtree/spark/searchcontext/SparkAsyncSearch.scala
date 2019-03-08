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
import com.github.robfitzgerald.dabtree.spark.sampler.SamplerDoublePrecision
import com.github.robfitzgerald.dabtree.spark.sampler.pedrosorei.{Payload, UCBPedrosoReiGlobalState, UCBPedrosoReiGlobals, UCBPedrosoReiMetaParameters}
import com.github.robfitzgerald.dabtree.spark.searchcontext.DoublePrecisionCollect.CollectResult

class SparkAsyncSearch[S, A](
  sparkContext: SparkContext,
  cores   : Int,
  partitions: Int,
  dabtreeFunctions: DabTreeFunctionParameters[S, A, Double],
  objective: Objective[Double],
  rankingPolicy: Payload[S, A, Double] => Double,
  activatedPayloadLimit: Int,
  totalPayloadCapacity: Int,
  startFrontier: List[(S, Option[A])],
  synchronize         : Boolean = true,
  checkpointRate      : Int = 10,
  explorationCoefficient: Int => Double = (_: Int) => math.sqrt(2),
  explorationUpdate   : Option[Double => Double] = None,
  expandObservationsThreshold: Int = 30,
  pStarPromotion             : Double = 0.5D,
  maxExpandPerIteration      : Int = 2,
  resultCollectionBufferTime: Long = 1000L
) {

  def run(iterationsMax: Int, durationMax: Long, samplesPerIteration: Int): Option[CollectResult[S]] = {

    // stop times relative to each node in this cluster, based on the currentTimeMillis of that cluster

    val bCastDuration = sparkContext.broadcast(durationMax - resultCollectionBufferTime)
//    val bCastStopTime = sparkContext.broadcast(System.currentTimeMillis() + durationMax)
    val stopTimes: Array[(Int, Long)] = sparkContext.
      parallelize(0 until cores * partitions, cores * partitions).
      map { index => (index, System.currentTimeMillis() + bCastDuration.value) }.
      collect()

    val startTime = System.currentTimeMillis()

    // globally-available
    val bCastObjective = sparkContext.broadcast(objective)
    val bCastSamplesPerIteration = sparkContext.broadcast(samplesPerIteration)

    // core DABTree Search Phases
    val bCastDabtreeFunctions = sparkContext.broadcast(dabtreeFunctions)
    val sampler: SamplerDoublePrecision[S, A] = SamplerDoublePrecision(bCastDabtreeFunctions, bCastObjective)
    val expandFunction: Payload[S, A, Double] => List[Payload[S, A, Double]] =
      SparkAsyncSearch.serializationSafeExpandFunction(
        expandObservationsThreshold,
        pStarPromotion,
        maxExpandPerIteration,
        objective,
        bCastDabtreeFunctions
      )
    val rebalanceFunction: List[Payload[S, A, Double]] => List[Payload[S, A, Double]] =
      SparkAsyncSearch.serializationSafeRebalancingFunction(activatedPayloadLimit, totalPayloadCapacity, rankingPolicy)
    val bCastSampler = sparkContext.broadcast(sampler)
    val bCastExpandFunction = sparkContext.broadcast(expandFunction)
    val bCastRebalanceFunction = sparkContext.broadcast(rebalanceFunction)

    // capture Cancellation data
    val cancelledPayloadAccumulator: CancelledPayloadAccumulator = new CancelledPayloadAccumulator()
    sparkContext.register(cancelledPayloadAccumulator, "CancelledPayloadAccumulator")

    // set up checkpoint directory
//    val checkpointDirectory = if (workingDirectory.endsWith("/")) s"${workingDirectory}checkpoint/" else s"$workingDirectory/checkpoint"
//    sparkContext.setCheckpointDir(checkpointDirectory)



    /**
      * runs each iteration of our algorithm, with break points in place to short-circuit for when we exceed our compute time
      *
      * @param frontier the group of payloads where we are concentrating our MCTS search
      * @param it       current iteration number
      * @return the final frontier.. and number of iterations
      */
    @tailrec
    def _run(frontier: RDD[Payload[S, A, Double]], it: Int = 1, sync: Int = 0): (RDD[Payload[S, A, Double]], Int, Int) = {
      if (it > iterationsMax) {
        (frontier, it - 1, sync)
      } else {
        if (it % checkpointRate == 0) {
          frontier.persist()
        }

        val stop: Boolean =
          if (it % SparkAsyncSearch.TerminationCheckRate == 0) {
            frontier.map{ case (_, _, stopTime) => stopTime < System.currentTimeMillis() }.fold(false){_||_}
          } else false

        if (stop) {
          (frontier, it - 1, sync)
        } else {
          ///////////////////////
          // 1 --- sample step //
          ///////////////////////
          val sampledFrontier: RDD[Payload[S, A, Double]] =
          frontier.
            map { payload =>
              val (parent, _, stopTime) = payload
              if (System.currentTimeMillis() > stopTime) {
                payload
              } else if (parent.searchState != SearchState.Activated) { // only sample activated nodes
                payload
              } else {
                bCastSampler.value.run(payload, bCastSamplesPerIteration.value).value
              }
            }
          //        println(s"sampled frontier of size ${sampledFrontier.collect.length}")

          ////////////////////////////
          // 2 --- synchronize step //
          ////////////////////////////
          val syncedFrontier: RDD[Payload[S, A, Double]] =
          if (!synchronize) sampledFrontier
          else SparkDoublePrecisionSynchronization.synchronize[S, A](sampledFrontier, bCastSampler, bCastObjective)

          //        println(s"sync frontier of size ${syncedFrontier.collect.length}")



          ///////////////////////////////////
          // 3 --- expand & rebalance step //
          ///////////////////////////////////
          val expandedFrontier: RDD[Payload[S, A, Double]] =
          syncedFrontier.
            flatMap { payload =>
              val (_, _, stopTime) = payload
              if (System.currentTimeMillis() > stopTime) { // exceeded time budget - no op
                Iterator(payload)
                //                  } else if (parent.searchState != SearchState.Activated) { // only expand children of Activated nodes - no op (wait, this isn't desired, is it?)
                //                    List(payload)
              } else {
                bCastExpandFunction.value(payload)
              }
            }

          val rebalancedFrontier: RDD[Payload[S, A, Double]] =
            expandedFrontier.
              mapPartitions { partitionIterator =>
                val partitionList = partitionIterator.toList
                if (partitionList.isEmpty) partitionIterator
                else {
                  val rebalanced: Iterator[Payload[S, A, Double]] = bCastRebalanceFunction.value(partitionList).toIterator
                  val noCancelled = rebalanced.flatMap{ payload =>
                    val (parent, _, _) = payload
                    if (parent.searchState == SearchState.Cancelled) {
                      val cancelledData = CancelledPayloadAccumulator.CancelledData(
                        parent.searchStats,
                        parent.mctsStats.observations
                      )
                      cancelledPayloadAccumulator.add(cancelledData)
                      Iterator.empty
                    } else {
                      Iterator(payload)
                    }
                  }
                  noCancelled
                }
              }

//          println(s"it $it frontier ${rebalancedFrontier.count()}")

          //        println(s"expand/rebalance frontier of size ${rebalancedFrontier.collect.length}")

          //        val collect = rebalancedFrontier.collect()
          //        val (count, act, sus, can, samples) = (
          //          collect.length,
          //          collect.count(_._1.searchState == SearchState.Activated),
          //          collect.count(_._1.searchState == SearchState.Suspended),
          //          collect.count(_._1.searchState == SearchState.Cancelled),
          //          if (collect.isEmpty) 0 else collect.map{_._1.mctsStats.observations}.sum
          //        )
          //        println(s"iteration $it with $count payloads (Act:$act Sus:$sus Can:$can), $samples total samples")

          val nextSync = if (synchronize) sync + 1 else sync
          _run(rebalancedFrontier, it + 1, nextSync)
        }
      }
    }


    // build starting payloads based on user-provided exploration coefficient function and objective
    val startFrontierPayloads: List[Payload[S, A, Double]] = {
      for {
        (index, stopTime) <- stopTimes
        (state, action)   <- startFrontier
      } yield {
        val newParent = SparkBanditParent.frontierPayload(
          state,
          action,
          Some(dabtreeFunctions.evaluate(_)),
          dabtreeFunctions.generateChildren,
          objective
        )
        val newGlobalState = UCBPedrosoReiGlobalState[S, A, Double](objective)
        val newGlobalMeta = UCBPedrosoReiMetaParameters(explorationCoefficient(index))
        (newParent, Some(UCBPedrosoReiGlobals[S, A, Double](newGlobalState, newGlobalMeta)), stopTime)
      }
    }.toList

    // run search
    val (searchResult: RDD[Payload[S, A, Double]], iterationsCount: Int, synchronizations: Int) = _run(sparkContext.parallelize(startFrontierPayloads, cores))

    val realDuration = System.currentTimeMillis() - startTime
    println(f"took ${realDuration.toDouble / 1000}%.2f seconds.")

    // return result
    DoublePrecisionCollect.collectDoublePrecision(searchResult, cancelledPayloadAccumulator.value, objective, iterationsCount, synchronizations)
  }
}

object SparkAsyncSearch {

  val TerminationCheckRate: Int = 3

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
