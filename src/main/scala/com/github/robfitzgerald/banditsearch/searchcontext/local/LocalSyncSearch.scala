package com.github.robfitzgerald.banditsearch.searchcontext.local

import scala.annotation.tailrec

import cats.{Id, Monad}

import spire.math.Numeric
import com.github.robfitzgerald.banditsearch.Objective
import com.github.robfitzgerald.banditsearch.banditnode.{BanditParent, SearchState}
import com.github.robfitzgerald.banditsearch.sampler.pedrosorei.{UCBPedrosoReiGlobalState, UCBPedrosoReiSampler}
import com.github.robfitzgerald.banditsearch.sampler.implicits._
import spire.algebra.Trig
import spire.implicits._

/**
  * runs
  */
class LocalSyncSearch[S, A, V: Numeric : Trig](
  simulate                      : S => S,
  evaluate                      : S => V,
  objective                     : Objective[V]
) {
  val Sampler: UCBPedrosoReiSampler[S,A,V] = UCBPedrosoReiSampler[S,A,V](simulate, evaluate, objective, math.sqrt(2))

  type Payload = (BanditParent[S, A, V], Option[UCBPedrosoReiGlobalState[S,A,V]])

  def run(iterations: Int, duration: Long, samples: Int): (S, V, Double, Int) = {
    val stopTime: Long = System.currentTimeMillis() + duration

    @tailrec
    def _run(frontier: List[this.Payload], it: Int = 1): List[this.Payload] = {
      if (it > iterations || System.currentTimeMillis() > stopTime) frontier
      else {
        val mctsSampledFrontier: List[this.Payload] =
          frontier.
            map { payload =>
              val (parent, globals) = payload
              if (parent.searchState == SearchState.Activated) {
                val updatedPayload: this.Payload = Sampler.run[Id]((parent, globals), samples)
                updatedPayload
              } else payload
            }

        if (it > iterations || System.currentTimeMillis() > stopTime) mctsSampledFrontier
        else {
          // todo: expand

          if (it > iterations || System.currentTimeMillis() > stopTime) mctsSampledFrontier
          else {
            // todo: partition pruning

            _run(mctsSampledFrontier, it + 1)
          }
        }

      }
    }

    // todo: generate top payload

    val searchResult = _run(List())

    // todo: evaluate result

    ???
  }
}

object LocalSyncSearch {
}