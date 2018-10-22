package com.github.robfitzgerald.banditsearch.sampler.pedrosorei

import cats.Monad
import cats.implicits._

import com.github.robfitzgerald.banditsearch.banditnode.BanditParent
import com.github.robfitzgerald.banditsearch.sampler.Sampler
import spire.algebra.Trig
import spire.math.Numeric

trait UCBPedrosoReiSamplerTypeclass {
  implicit def UCBPedrosoReiSamplerOps[S, A, V : Numeric : Trig]: Sampler[UCBPedrosoReiSampler[S,A,V]] =
    new Sampler[UCBPedrosoReiSampler[S,A,V]] {

      def run[F[_] : Monad](sampler: UCBPedrosoReiSampler[S, A, V], iterations: Int): F[UCBPedrosoReiSampler[S, A, V]] = {

        val work: F[(BanditParent[S,A,V], UCBPedrosoReiSamplerState[S,A,V])] =
          Sampler
            .run[F,S,A,V,UCBPedrosoReiSamplerState[S,A,V]](
              sampler.parent,
              iterations,
              sampler.samplerState,
              sampler.randomSelection,
              sampler.simulate,
              sampler.evaluate,
              sampler.updateStats,
              sampler.updateSamplerState,
              sampler.rewardFunction
            )

        for {
          result <- work
        } yield {
          val (updatedParent, updatedSamplerState) = result
          sampler.copy(
            parent = updatedParent,
            samplerState = updatedSamplerState
          )
        }
      }
    }
}
