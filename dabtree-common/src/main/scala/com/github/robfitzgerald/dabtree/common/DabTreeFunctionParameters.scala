package com.github.robfitzgerald.dabtree.common

trait DabTreeFunctionParameters[State, Action, Value] extends Serializable {
  def simulate(state: State): State
  def evaluate(state: State): Value
  def generateChildren(state: State): Array[(State, Option[Action])]
  def allowChildExpansion(state: State): Boolean
}
