package com.github.robfitzgerald.banditsearch

import com.github.robfitzgerald.banditsearch.banditnode.HasMCTSStats
import spire.algebra.Order

trait Objective[V] {
  def zero: V

  def badBounds: V

  def optimalBounds: V

  def optimal(a: V, b: V): V

  def bestSimulation(node: HasMCTSStats[V]): V
}

object Objective {

  case class Minimize[V: Order](optimalBounds: V, badBounds: V)(implicit val ordering: Order[V]) extends Objective[V] {
    require(ordering.compare(optimalBounds, badBounds) < 0, s"Minimize objective should have a 'optimalBounds' < 'badBounds' (since min is optimal), but instead $optimalBounds >= $badBounds.")

    override def zero: V = badBounds

    override def optimal(a: V, b: V): V = if (ordering.compare(a, b) < 0) a else b

    override def bestSimulation(node: HasMCTSStats[V]): V = node.mctsStats.min
  }

  case class Maximize[V: Order](badBounds: V, optimalBounds: V)(implicit val ordering: Order[V]) extends Objective[V] {
    require(ordering.compare(optimalBounds, badBounds) > 0, s"Maximize objective should have a 'optimalBounds' > 'badBounds' (since max is optimal), but instead $optimalBounds <= $badBounds.")

    override def zero: V = badBounds

    override def optimal(a: V, b: V): V = if (ordering.compare(a, b) > 0) a else b

    override def bestSimulation(node: HasMCTSStats[V]): V = node.mctsStats.max
  }

}
