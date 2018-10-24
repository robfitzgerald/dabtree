package com.github.robfitzgerald.banditsearch.searchcontext.local

import scala.annotation.tailrec

import cats.Id
import cats.implicits._

import com.github.robfitzgerald.banditsearch.Objective
import com.github.robfitzgerald.banditsearch.banditnode.{BanditParent, SearchState}
import com.github.robfitzgerald.banditsearch.sampler.implicits._
import com.github.robfitzgerald.banditsearch.sampler.pedrosorei.{UCBPedrosoReiGlobals, UCBPedrosoReiSampler}
import com.github.robfitzgerald.banditsearch.searchcontext.{GenericPedrosoReiExpansion, GenericPedrosoReiSynchronization}
import spire.algebra.Trig
import spire.math.Numeric


/**
  * runs a search executed in the standard library List container and in the Id effect type
  */
class LocalSyncSearch[S, A, V: Numeric : Trig](
  simulate              : S => S,
  evaluate              : S => V,
  generateChildren      : S => Array[(S, Option[A])],
  objective             : Objective[V],
  allowChildExpansion   : S => Boolean,
  synchronize           : Boolean = true,
  explorationCoefficient: Double = math.sqrt(2),
  observationsThreshold : Int = 30,
  rewardThreshold       : Double = 0.5D,
  maxExpandPerIteration : Int = 2
) {

  val Sampler: UCBPedrosoReiSampler[S, A, V] = UCBPedrosoReiSampler[S, A, V](simulate, evaluate, objective)
  implicit val objectiveImplicit: Objective[V] = objective

  type Payload = (BanditParent[S, A, V], Option[UCBPedrosoReiGlobals[S, A, V]])

  def run(iterationsMax: Int, durationMax: Long, samplesPerIteration: Int): (S, V, Double, Int) = {
    val stopTime: Long = System.currentTimeMillis() + durationMax

    val expandFunction: Payload => List[Payload] =
      GenericPedrosoReiExpansion.expand[List, S, A, V](
        observationsThreshold,
        rewardThreshold,
        maxExpandPerIteration,
        allowChildExpansion,
        Some(evaluate),
        generateChildren
      )

    /**
      * runs each iteration of our algorithm, with break points in place to short-circuit for when we exceed our compute time
      *
      * @param frontier the group of payloads where we are concentrating our MCTS search
      * @param it       current iteration number
      * @return the final frontier.. and number of iterations
      */
    @tailrec
    def _run(frontier: List[this.Payload], it: Int = 1): (List[this.Payload], Int) = {

      if (it > iterationsMax || System.currentTimeMillis() > stopTime) (frontier, it - 1)
      else {

        ///////////////////////
        // 1 --- sample step //
        ///////////////////////
        val sampledFrontier: List[this.Payload] =
        frontier.
          map { payload =>
            val (parent, globals) = payload
            if (parent.searchState == SearchState.Activated) {
              val updatedPayload: this.Payload = Sampler.run[Id]((parent, globals), samplesPerIteration)
              updatedPayload
            } else payload
          }

        if (it > iterationsMax || System.currentTimeMillis() > stopTime) (sampledFrontier, it)
        else {

          ////////////////////////////
          // 2 --- synchronize step //
          ////////////////////////////
          val syncedFrontier: List[this.Payload] =
          if (!synchronize) sampledFrontier
          else GenericPedrosoReiSynchronization.synchronize[List, S, A, V](sampledFrontier, Sampler)

          if (it > iterationsMax || System.currentTimeMillis() > stopTime) (syncedFrontier, it)
          else {

            ///////////////////////
            // 3 --- expand step //
            ///////////////////////
            val expandedFrontier: List[this.Payload] =
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

              // todo: partition pruning

              _run(expandedFrontier, it + 1)
            }
          }
        }
      }
    }

    // todo: generate top payload

    val (searchResult, iterationsExecuted) = _run(List())

    // todo: evaluate result

    ???
  }
}

object LocalSyncSearch {
}