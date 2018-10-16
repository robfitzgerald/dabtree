package com.github.robfitzgerald.banditsearch

import spire.math.Fractional
import com.github.robfitzgerald.banditsearch.banditnode.BanditNode
import spire.algebra.Order

trait Objective [V <: Fractional[V]] {
  def zero: V
  def optimal(a: V, b: V): V
  def bestSimulation(node: BanditNode[_,_,V,_]): V
}

object Objective {
  case class Minimize [V <: Fractional[V]] (lowerBounds: V, upperBounds: V)(implicit val ordering: Order[V]) extends Objective[V] {
    override def zero: V = upperBounds
    override def optimal(a: V, b: V): V = if (ordering.compare(a, b) < 0) a else b
    override def bestSimulation(node: BanditNode[_,_,V,_]): V = node.mctsStats.min
}

  case class Maximize [V <: Fractional[V]] (lowerBounds: V, upperBounds: V)(implicit val ordering: Order[V]) extends Objective[V] {
    override def zero: V = lowerBounds
    override def optimal(a: V, b: V): V = if (ordering.compare(a, b) > 0) a else b
    override def bestSimulation(node: BanditNode[_,_,V,_]): V = node.mctsStats.max
  }
}
