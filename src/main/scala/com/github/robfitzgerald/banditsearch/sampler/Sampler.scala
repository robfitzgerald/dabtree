package com.github.robfitzgerald.banditsearch.sampler

import cats.Monad

import com.github.robfitzgerald.banditsearch.banditnode.{BanditChild, BanditParent}
import com.github.robfitzgerald.banditsearch.mctsstats.mutable.MCTSStatsMutableImpl
import spire.implicits._
import spire.math.Numeric

trait Sampler [A, P] {
  def run[F[_] : Monad](sampler: A, payload: P, iterations: Int): F[P]
}

object Sampler extends SamplerOps {
  def run[F[_] : Monad, S, A, V : Numeric, SamplerState](
    parent            : BanditParent[S,A,V],
    sampleIterations  : Int,
    samplerState      : SamplerState,
    randomSelection   : BanditParent[S,A,V] => Int,
    simulate          : S => S,
    evaluate          : S => V,
    updateStats       : (MCTSStatsMutableImpl[V], V) => Unit,
    updateSamplerState: (SamplerState, V) => Unit,
    rewardFunction    : (MCTSStatsMutableImpl[V], SamplerState, Int) => Double
  ): F[(BanditParent[S,A,V], SamplerState)] = {
    Monad[F].pure {

      // convert our data structures from immutable to mutable structures
      var currentSamplerState: SamplerState = samplerState
      var parentReward: Double = parent.reward
      var parentStats: MCTSStatsMutableImpl[V] = parent.mctsStats.toMutable
      var childrenRewards: collection.mutable.Buffer[Double] = parent.children.map{ _.reward }.toBuffer
      val childrenStats: Array[MCTSStatsMutableImpl[V]] = parent.children.map { _.mctsStats.toMutable }
      val childrenStates: IndexedSeq[S] = parent.children.map { _.state }

      // perform samples
      cfor(0)(_ < sampleIterations, _ + 1) { _ =>

        // basic MCTS sampling
        val selectedChildIndex: Int = randomSelection(parent)
        val selectedState: S = childrenStates(selectedChildIndex)
        val simulation: S = simulate(selectedState)
        val cost: V = evaluate(simulation)

        // perform update on child and parent
        val selectedChildStats = childrenStats(selectedChildIndex)
        updateSamplerState(currentSamplerState, cost)
        updateStats(selectedChildStats, cost)
        updateStats(parentStats, cost)
        childrenRewards(selectedChildIndex) = rewardFunction(selectedChildStats, currentSamplerState, parentStats.observations)
        parentReward = rewardFunction(childrenStats(selectedChildIndex), currentSamplerState, 0)
      }

      // back to immutable structures. update all children with sampling results
      val updatedChildren: Array[BanditChild[S, A, V]] =
        parent.children.
          zip(childrenRewards.zip(childrenStats)).
          map { case (child, (updatedReward, stats)) =>
            child.copy[S, A, V](
              reward = updatedReward,
              mctsStats = stats.toImmutable
            )
          }

      // update parent with updated children and sampling results
      val updatedParent = parent.copy[S, A, V](
        reward = parentReward,
        mctsStats = parentStats.toImmutable,
        children = updatedChildren
      )

      (updatedParent, currentSamplerState)
    }
  }
}

trait SamplerOps {
  implicit class SamplerTypeclass[A, P](sampler: A)(implicit evidence: Sampler[A, P]) {
    def run[F[_] : Monad](payload: P, iterations: Int): F[P] = evidence.run(sampler, payload, iterations)
  }
}