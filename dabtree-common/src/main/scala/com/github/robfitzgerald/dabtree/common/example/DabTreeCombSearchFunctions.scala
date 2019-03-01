package com.github.robfitzgerald.dabtree.common.example

import scala.util.Random

import com.github.robfitzgerald.dabtree.common.DabTreeFunctionParameters

class DabTreeCombSearchFunctions(
  realSolution    : Vector[Double],
  possibleValues  : Vector[Double],
  numChildren     : Int,
  expandLimit     : Int,
  random          : Random
) extends DabTreeFunctionParameters[Vector[Double], Double, Double] {
  // randomly fill our vector
  def simulate(state: Vector[Double]): Vector[Double] =  {
    state ++ {
      for {
        _ <- 1 to (realSolution.size - state.size)
        n = possibleValues(random.nextInt(possibleValues.size))
      } yield {
        n
      }
    }
  }

  // evaluate as the Euclidian distance, which we wish to minimize
  def evaluate(state: Vector[Double]): Double = {
    val sum: Double = state.
      zip(realSolution).
      map { case (a, b) => math.pow(a - b, 2) }.
      fold(0D) { (sum, n) => sum + n }
    math.sqrt(sum)
  }

  // generate values in the range [0, 1] stepping by 0.05 (21 values)
  def generateChildren(state: Vector[Double]): Array[(Vector[Double], Option[Double])] = {
    {
      for {
        n <- 1 to numChildren
        ratio = n.toDouble / numChildren
      } yield (state :+ ratio, Some(ratio))
    }.toArray
  }

  // allow up to the user-provided expand limit
  def allowChildExpansion(state: Vector[Double]): Boolean = {
    val expLim = expandLimit
    val canExpand = state.size < expLim
    if (canExpand) {
      true
    } else {
      false
    }
  }
}

object DabTreeCombSearchFunctions {
  def apply(
    realSolution    : Vector[Double],
    possibleValues  : Vector[Double],
    numChildren     : Int,
    expandLimit     : Int,
    random          : Random
  ): DabTreeCombSearchFunctions = new DabTreeCombSearchFunctions(
    realSolution,
    possibleValues,
    numChildren,
    expandLimit,
    random
  )
}