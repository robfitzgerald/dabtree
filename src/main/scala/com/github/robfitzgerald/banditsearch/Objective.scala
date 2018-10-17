package com.github.robfitzgerald.banditsearch

import com.github.robfitzgerald.banditsearch.banditnode.HasMCTSStats
import spire.algebra.Order

trait Objective [V] {
  def zero: V
  def optimal(a: V, b: V): V
  def bestSimulation(node: HasMCTSStats[V]): V
}

object Objective {
  case class Minimize [V : Order] (lowerBounds: V, upperBounds: V)(implicit val ordering: Order[V]) extends Objective[V] {
    override def zero: V = upperBounds
    override def optimal(a: V, b: V): V = if (ordering.compare(a, b) < 0) a else b
    override def bestSimulation(node: HasMCTSStats[V]): V = node.mctsStats.min
}

  case class Maximize [V : Order] (lowerBounds: V, upperBounds: V)(implicit val ordering: Order[V]) extends Objective[V] {
    override def zero: V = lowerBounds
    override def optimal(a: V, b: V): V = if (ordering.compare(a, b) > 0) a else b
    override def bestSimulation(node: HasMCTSStats[V]): V = node.mctsStats.max
  }
}
