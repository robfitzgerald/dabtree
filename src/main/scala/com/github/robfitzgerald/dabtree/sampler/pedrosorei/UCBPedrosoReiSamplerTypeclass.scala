package com.github.robfitzgerald.dabtree.sampler.pedrosorei

import cats.Monad
import cats.implicits._

import com.github.robfitzgerald.dabtree.banditnode.BanditParent
import com.github.robfitzgerald.dabtree.sampler.GenericSampler
import spire.algebra.Trig
import spire.math.Numeric

trait UCBPedrosoReiSamplerTypeclass {

  implicit def UCBPedrosoReiSamplerOps[S, A, V : Numeric : Trig]: GenericSampler[UCBPedrosoReiSampler[S,A,V], (BanditParent[S, A, V], Option[UCBPedrosoReiGlobals[S, A, V]])] =
    new GenericSampler[UCBPedrosoReiSampler[S,A,V], (BanditParent[S, A, V], Option[UCBPedrosoReiGlobals[S, A, V]])] {
      def run[F[_] : Monad](
        sampler: UCBPedrosoReiSampler[S, A, V],
        payload: (BanditParent[S, A, V], Option[UCBPedrosoReiGlobals[S, A, V]]),
        samples: Int
      ): F[(BanditParent[S, A, V], Option[UCBPedrosoReiGlobals[S, A, V]])] = {
        val (parent: BanditParent[S, A, V], globalsOption: Option[UCBPedrosoReiGlobals[S, A, V]]) = payload
        globalsOption match {
          case None =>
            Monad[F].pure{payload}

          case Some(globals) =>
            for {
              result <- GenericSampler
                          .run[F,S,A,V,UCBPedrosoReiGlobals[S,A,V]](
                            parent,
                            samples,
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
