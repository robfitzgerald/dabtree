package com.github.robfitzgerald.banditsearch.sampler.pedrosorei

import cats.Monad
import cats.implicits._

import com.github.robfitzgerald.banditsearch.banditnode.BanditParent
import com.github.robfitzgerald.banditsearch.sampler.Sampler
import spire.algebra.Trig
import spire.math.Numeric

trait UCBPedrosoReiSamplerTypeclass {

  implicit def UCBPedrosoReiSamplerOps[S, A, V : Numeric : Trig]: Sampler[UCBPedrosoReiSampler[S,A,V], (BanditParent[S, A, V], Option[UCBPedrosoReiGlobals[S, A, V]])] =
    new Sampler[UCBPedrosoReiSampler[S,A,V], (BanditParent[S, A, V], Option[UCBPedrosoReiGlobals[S, A, V]])] {
      def run[F[_] : Monad](
        sampler: UCBPedrosoReiSampler[S, A, V],
        payload: (BanditParent[S, A, V], Option[UCBPedrosoReiGlobals[S, A, V]]),
        iterations: Int
      ): F[(BanditParent[S, A, V], Option[UCBPedrosoReiGlobals[S, A, V]])] = {
        val (parent: BanditParent[S, A, V], globalsOption: Option[UCBPedrosoReiGlobals[S, A, V]]) = payload
        globalsOption match {
          case None =>
            Monad[F].pure{payload}

          case Some(globals) =>
            for {
              result <- Sampler
                          .run[F,S,A,V,UCBPedrosoReiGlobals[S,A,V]](
                            parent,
                            iterations,
                            globals,
                            sampler.randomSelection,
                            sampler.simulate,
                            sampler.evaluate,
                            sampler.updateStats,
                            sampler.updateSamplerState,
                            sampler.rewardFunction
                          )
            } yield {
              (result._1, Some(result._2))
            }
        }
      }
    }
}
