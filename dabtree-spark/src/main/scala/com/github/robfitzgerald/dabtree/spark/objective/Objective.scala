package com.github.robfitzgerald.dabtree.spark.objective

/**
  * captures the orientation of our optimization, which contains monoidal operations over this orientation
  *
  */
trait Objective[V] extends Serializable {
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


