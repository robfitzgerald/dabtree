package com.github.robfitzgerald.dabtree.local.sampler

import cats.{Eval, Monad}

import com.github.robfitzgerald.dabtree.local.banditnode.{BanditChild, BanditParent}
import com.github.robfitzgerald.dabtree.local.mctsstats.mutable.MCTSStatsMutableImpl
import spire.implicits._
import spire.math.Numeric

trait GenericSampler [A, P] {
  def run(sampler: A, payload: P, samples: Int): Eval[P]
}

object GenericSampler extends SamplerOps with Serializable {
  def run[S, A, V : Numeric, SamplerState](
    parent            : BanditParent[S,A,V],
    sampleIterations  : Int,
    samplerState      : SamplerState,
    randomSelection   : BanditParent[S,A,V] => Int,
    simulate          : S => S,
    evaluate          : S => V,
    updateStats       : (MCTSStatsMutableImpl[V], V) => Unit,
    updateSamplerState: (SamplerState, S, A, V) => SamplerState,
    rewardFunction    : (MCTSStatsMutableImpl[V], SamplerState, Int) => Double
  ): Eval[(BanditParent[S,A,V], SamplerState)] = {
    Eval.now {

      // convert our data structures from immutable to mutable structures
      var updatedSamplerState: SamplerState = samplerState
      var parentReward: Double = parent.reward
      val parentStats: MCTSStatsMutableImpl[V] = parent.mctsStats.toMutable
      val childrenRewards: collection.mutable.Buffer[Double] = parent.children.map{ _.reward }.toBuffer
      val childrenStats: Array[MCTSStatsMutableImpl[V]] = parent.children.map { _.mctsStats.toMutable }
      val childrenActions: IndexedSeq[A] = parent.children.flatMap { _.action }
      val childrenStates: IndexedSeq[S] = parent.children.map { _.state }

      // perform samples
      var bestIdx = 0
      var bestReward = 0.0
      cfor(0)(_ < sampleIterations, _ + 1) { _ =>

        // basic MCTS sampling
//        val selectedChildIndex: Int = randomSelection(parent)
        val selectedAction: A = childrenActions(bestIdx)
        val selectedState: S = childrenStates(bestIdx)
        val simulatedState: S = simulate(selectedState)
        val cost: V = evaluate(simulatedState)

        // perform update on child and parent (globals via immutable semantics, parent/child via mutable)
        val selectedChildStats = childrenStats(bestIdx)
        updatedSamplerState = updateSamplerState(updatedSamplerState, simulatedState, selectedAction, cost)
        updateStats(selectedChildStats, cost)
        updateStats(parentStats, cost)

        // update all rewards, and note the best child along the way, for the next sample
        bestReward = 0.0
        for {
          childIdx <- childrenRewards.indices
        } {
          val updateChild = childrenStats(childIdx)
          val childRewardUpdate: Double = rewardFunction(updateChild, updatedSamplerState, parentStats.observations)
          if (bestReward < childRewardUpdate) {
            bestReward = childRewardUpdate
            bestIdx = childIdx
          }
          childrenRewards.update(childIdx, childRewardUpdate)
        }
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
  implicit class SamplerTypeclass[A, P](sampler: A)(implicit evidence: GenericSampler[A, P]) {
    def run(payload: P, samples: Int): Eval[P] = evidence.run(sampler, payload, samples)
  }
}