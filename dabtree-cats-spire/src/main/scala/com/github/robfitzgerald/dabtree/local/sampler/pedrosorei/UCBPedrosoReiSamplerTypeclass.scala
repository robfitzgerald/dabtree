package com.github.robfitzgerald.dabtree.local.sampler.pedrosorei

import scala.util.Random

import cats.Eval

import com.github.robfitzgerald.dabtree.local.banditnode.BanditParent
import com.github.robfitzgerald.dabtree.local.sampler.GenericSampler
import spire.algebra.Trig
import spire.math.Numeric

trait UCBPedrosoReiSamplerTypeclass {

  implicit def UCBPedrosoReiSamplerOps[S, A, V : Numeric : Trig]: GenericSampler[UCBPedrosoReiSampler[S,A,V], (BanditParent[S, A, V], Option[UCBPedrosoReiGlobals[S, A, V]], Random)] =
    new GenericSampler[UCBPedrosoReiSampler[S,A,V], (BanditParent[S, A, V], Option[UCBPedrosoReiGlobals[S, A, V]], Random)] {
      def run(
        sampler: UCBPedrosoReiSampler[S, A, V],
        payload: (BanditParent[S, A, V], Option[UCBPedrosoReiGlobals[S, A, V]], Random),
        samples: Int
      ): Eval[(BanditParent[S, A, V], Option[UCBPedrosoReiGlobals[S, A, V]], Random)] = {
        val (parent: BanditParent[S, A, V], globalsOption: Option[UCBPedrosoReiGlobals[S, A, V]], random) = payload
        globalsOption match {
          case None =>
            Eval.now{payload}

          case Some(globals) =>
            for {
              result <- GenericSampler
                          .run[S,A,V,UCBPedrosoReiGlobals[S,A,V]](
                            parent,
                            samples,
                            globals,
                            sampler.randomSelection,
                            sampler.simulate,
                            sampler.evaluate,
                            sampler.updateStats,
                            sampler.updateSamplerState,
                            sampler.rewardFunction,
                            random
                          )
            } yield {
              (result._1, Some(result._2), random)
            }
        }
      }
    }
}
