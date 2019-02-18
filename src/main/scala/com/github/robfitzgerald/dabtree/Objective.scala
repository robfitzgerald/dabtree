package com.github.robfitzgerald.dabtree

import spire.algebra.Order

/**
  * captures the orientation of our optimization, which contains monoidal operations over this orientation
  * @tparam V the value type
  */
trait Objective[V] {
  /**
    * the numeric representation of a zero-value in our objective
    * @return the zero value
    */
  def zero: V

  /**
    * the numeric bounds which is the extreme value of bad costs
    * @return bad value bounds
    */
  def badBounds: V

  /**
    * the numeric bounds which is the extreme value of good costs
    * @return good value bounds
    */
  def optimalBounds: V

  /**
    * returns the more optimal value
    * @param a the first value
    * @param b the second value
    * @return the more optimal value
    */
  def optimal(a: V, b: V): V

  /**
    * returns the more optimal value, where the left value is known to be lower than the right.
    * @param min the lower value
    * @param max the higher value
    * @return the more optimal value
    */
  def bestSimulation(min: V, max: V): V

  /**
    * predicate for finding the more optimal value
    * @param a the first value
    * @param b the second value
    * @return true when the first value is more optimal than the second value
    */
  def isMoreOptimalThan(a: V, b: V): Boolean
}

object Objective {

  case class Minimize[V: Order](optimalBounds: V, badBounds: V)(implicit val ordering: Order[V]) extends Objective[V] {
    require(ordering.compare(optimalBounds, badBounds) < 0, s"Minimize objective should have a 'optimalBounds' < 'badBounds' (since min is optimal), but instead $optimalBounds >= $badBounds.")

    override def zero: V = badBounds

    override def optimal(a: V, b: V): V = if (isMoreOptimalThan(a,b)) a else b

    override def bestSimulation(min: V, max: V): V = min

    override def isMoreOptimalThan(a: V, b: V): Boolean = ordering.compare(a, b) < 0
  }

  case class Maximize[V: Order](badBounds: V, optimalBounds: V)(implicit val ordering: Order[V]) extends Objective[V] {
    require(ordering.compare(optimalBounds, badBounds) > 0, s"Maximize objective should have a 'optimalBounds' > 'badBounds' (since max is optimal), but instead $optimalBounds <= $badBounds.")

    override def zero: V = badBounds

    override def optimal(a: V, b: V): V = if (isMoreOptimalThan(a,b)) a else b

    override def bestSimulation(min: V, max: V): V = max

    override def isMoreOptimalThan(a: V, b: V): Boolean = ordering.compare(a, b) > 0
  }

}
