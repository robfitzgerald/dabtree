package com.github.robfitzgerald.dabtree.local.searchcontext.local

import scala.annotation.tailrec
import scala.util.Random

import cats.implicits._

import com.github.robfitzgerald.dabtree.local.banditnode.BanditParent
import com.github.robfitzgerald.dabtree.common.banditnode.SearchState
import com.github.robfitzgerald.dabtree.local.Objective
import com.github.robfitzgerald.dabtree.local.sampler.pedrosorei.Payload
import com.github.robfitzgerald.dabtree.local.sampler.implicits._
import com.github.robfitzgerald.dabtree.local.sampler.pedrosorei.{UCBPedrosoReiGlobalState, UCBPedrosoReiGlobals, UCBPedrosoReiMetaParameters, UCBPedrosoReiSampler}
import com.github.robfitzgerald.dabtree.local.searchcontext.GenericPedrosoReiCollect.CollectResult
import com.github.robfitzgerald.dabtree.local.searchcontext.{GenericPedrosoReiCollect, GenericPedrosoReiExpansion, GenericPedrosoReiPartitionRebalancing, GenericPedrosoReiSynchronization}
import spire.algebra.Trig
import spire.math.Numeric


/**
  * runs a search executed in the standard library List container and in the Id effect type
  */
class LocalSyncSearch[S, A, V: Numeric : Trig](
  simulate                   : (S, Random) => S,
  evaluate                   : S => V,
  objective                  : Objective[V],
  generateChildren           : S => Array[(S, Option[A])],
  rankingPolicy              : Payload[S, A, V] => Double,
  allowChildExpansion        : S => Boolean,
  activatedPayloadLimit      : Int,
  totalPayloadCapacity       : Int,
  startFrontier              : List[(S, Option[A])],
  synchronize                : Boolean = true,
  explorationCoefficient     : Int => Double = (_: Int) => math.sqrt(2),
  explorationUpdate          : Option[Double => Double] = None,
  expandObservationsThreshold: Int = 30,
  pStarPromotion             : Double = 0.5D,
  maxExpandPerIteration      : Int = 2,
  seed: Long = 0
) {

  val topRandom: Random = new Random(seed)
  val Sampler: UCBPedrosoReiSampler[S, A, V] = UCBPedrosoReiSampler[S, A, V](simulate, evaluate, objective)
  implicit val objectiveImplicit: Objective[V] = objective

  def run(iterationsMax: Int, durationMax: Long, samplesPerIteration: Int): Option[CollectResult[S, V]] = {
    val stopTime: Long = System.currentTimeMillis() + durationMax

    val expandFunction: Payload[S, A, V] => List[Payload[S, A, V]] =
      GenericPedrosoReiExpansion.expand[List, S, A, V](
        expandObservationsThreshold,
        pStarPromotion,
        maxExpandPerIteration,
        objective,
        allowChildExpansion,
        Some(evaluate),
        generateChildren
      )

    val rebalanceFunction: List[Payload[S, A, V]] => List[Payload[S, A, V]] =
      GenericPedrosoReiPartitionRebalancing.rebalance[List, S, A, V](
        activatedPayloadLimit,
        totalPayloadCapacity,
        rankingPolicy
      )

    /**
      * runs each iteration of our algorithm, with break points in place to short-circuit for when we exceed our compute time
      *
      * @param frontier the group of payloads where we are concentrating our MCTS search
      * @param it       current iteration number
      * @return the final frontier.. and number of iterations
      */
    @tailrec
    def _run(frontier: List[Payload[S, A, V]], it: Int = 1): (List[Payload[S, A, V]], Int) = {

      if (it > iterationsMax || System.currentTimeMillis() > stopTime) (frontier, it - 1)
      else {

        ///////////////////////
        // 1 --- sample step //
        ///////////////////////
        val sampledFrontier: List[Payload[S, A, V]] =
          frontier.
            map { case (parent, globals, random) =>
              if (parent.searchState == SearchState.Activated) {
                val updatedPayload: Payload[S, A, V] = Sampler.run((parent, globals, random), samplesPerIteration).value
//                print("^")
                updatedPayload
              } else {
//                print("_")
                (parent, globals, random)
              }
            }
//        print("\n")

        if (it > iterationsMax || System.currentTimeMillis() > stopTime) (sampledFrontier, it)
        else {

          ////////////////////////////
          // 2 --- synchronize step //
          ////////////////////////////
          val syncedFrontier: List[Payload[S, A, V]] =
            if (!synchronize) sampledFrontier
            else GenericPedrosoReiSynchronization.synchronize[List, S, A, V](sampledFrontier, Sampler)

          if (it > iterationsMax || System.currentTimeMillis() > stopTime) (syncedFrontier, it)
          else {

            ///////////////////////
            // 3 --- expand step //
            ///////////////////////
            val expandedFrontier: List[Payload[S, A, V]] =
            syncedFrontier.
              flatMap { payload =>
                if (payload._1.searchState != SearchState.Activated) {
                  List(payload)
                } else {
                  expandFunction(payload)
                }
              }

            if (it > iterationsMax || System.currentTimeMillis() > stopTime) (expandedFrontier, it)
            else {

              //////////////////////////
              // 4 --- rebalance step //
              //////////////////////////
              val rebalancedFrontier: List[Payload[S, A, V]] = rebalanceFunction(expandedFrontier)

              _run(rebalancedFrontier, it + 1)
            }
          }
        }
      }
    }

    // build starting payloads based on user-provided exploration coefficient function and objective
    val startFrontierPayloads: List[Payload[S, A, V]] =
      startFrontier.
        zipWithIndex.
          map{ case ((state, action), index: Int) =>
            val newParent = BanditParent.frontierPayload(
              state,
              action,
              Some(evaluate),
              generateChildren,
              objective
            )
            val newGlobalState = UCBPedrosoReiGlobalState[S, A, V](objective)
            val newGlobalMeta = UCBPedrosoReiMetaParameters(explorationCoefficient(index))
            val random: Random = new Random(topRandom.nextLong())
          (newParent, Some(UCBPedrosoReiGlobals[S, A, V](newGlobalState, newGlobalMeta)), random)
        }

    val (searchResult, iterationsCount) = _run(startFrontierPayloads)

    GenericPedrosoReiCollect.collect(searchResult, objective, iterationsCount)
  }
}