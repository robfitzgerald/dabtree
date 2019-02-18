package com.github.robfitzgerald.dabtree.sampler

import cats.Monad

import com.github.robfitzgerald.dabtree.banditnode.{BanditChild, BanditParent}
import com.github.robfitzgerald.dabtree.mctsstats.mutable.MCTSStatsMutableImpl
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
    updateSamplerState: (SamplerState, S, A, V) => SamplerState,
    rewardFunction    : (MCTSStatsMutableImpl[V], SamplerState, Int) => Double
  ): F[(BanditParent[S,A,V], SamplerState)] = {
    Monad[F].pure {

      // convert our data structures from immutable to mutable structures
      var updatedSamplerState: SamplerState = samplerState
      var parentReward: Double = parent.reward
      val parentStats: MCTSStatsMutableImpl[V] = parent.mctsStats.toMutable
      val childrenRewards: collection.mutable.Buffer[Double] = parent.children.map{ _.reward }.toBuffer
      val childrenStats: Array[MCTSStatsMutableImpl[V]] = parent.children.map { _.mctsStats.toMutable }
      val childrenActions: IndexedSeq[A] = parent.children.flatMap { _.action }
      val childrenStates: IndexedSeq[S] = parent.children.map { _.state }

      // perform samples
      cfor(0)(_ < sampleIterations, _ + 1) { _ =>

        // basic MCTS sampling
        val selectedChildIndex: Int = randomSelection(parent)
        val selectedAction: A = childrenActions(selectedChildIndex)
        val selectedState: S = childrenStates(selectedChildIndex)
        val simulatedState: S = simulate(selectedState)
        val cost: V = evaluate(simulatedState)

        // perform update on child and parent (globals via immutable semantics, parent/child via mutable)
        val selectedChildStats = childrenStats(selectedChildIndex)
        updatedSamplerState = updateSamplerState(updatedSamplerState, simulatedState, selectedAction, cost)
        updateStats(selectedChildStats, cost)
        updateStats(parentStats, cost)
        val childRewardUpdate = rewardFunction(selectedChildStats, updatedSamplerState, parentStats.observations)
        childrenRewards.update(selectedChildIndex, childRewardUpdate)
        parentReward = rewardFunction(parentStats, updatedSamplerState, 0)
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

      (updatedParent, updatedSamplerState)
    }
  }
}

trait SamplerOps {
  implicit class SamplerTypeclass[A, P](sampler: A)(implicit evidence: Sampler[A, P]) {
    def run[F[_] : Monad](payload: P, iterations: Int): F[P] = evidence.run(sampler, payload, iterations)
  }
}