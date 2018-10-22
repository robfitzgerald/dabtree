package com.github.robfitzgerald.banditsearch

import spire.algebra.Order

trait Objective[V] {
  def zero: V

  def badBounds: V

  def optimalBounds: V

  def optimal(a: V, b: V): V

  def bestSimulation(min: V, max: V): V

  def isBetterThan(a: V, b: V): Boolean
}

object Objective {

  case class Minimize[V: Order](optimalBounds: V, badBounds: V)(implicit val ordering: Order[V]) extends Objective[V] {
    require(ordering.compare(optimalBounds, badBounds) < 0, s"Minimize objective should have a 'optimalBounds' < 'badBounds' (since min is optimal), but instead $optimalBounds >= $badBounds.")

    override def zero: V = badBounds

    override def optimal(a: V, b: V): V = if (isBetterThan(a,b)) a else b

    override def bestSimulation(min: V, max: V): V = min

    override def isBetterThan(a: V, b: V): Boolean = ordering.compare(a, b) < 0
  }

  case class Maximize[V: Order](badBounds: V, optimalBounds: V)(implicit val ordering: Order[V]) extends Objective[V] {
    require(ordering.compare(optimalBounds, badBounds) > 0, s"Maximize objective should have a 'optimalBounds' > 'badBounds' (since max is optimal), but instead $optimalBounds <= $badBounds.")

    override def zero: V = badBounds

    override def optimal(a: V, b: V): V = if (isBetterThan(a,b)) a else b

    override def bestSimulation(min: V, max: V): V = max

    override def isBetterThan(a: V, b: V): Boolean = ordering.compare(a, b) > 0
  }

}
