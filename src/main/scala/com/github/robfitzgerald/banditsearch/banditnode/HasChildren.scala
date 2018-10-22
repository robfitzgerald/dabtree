package com.github.robfitzgerald.banditsearch.banditnode

import com.github.robfitzgerald.banditsearch.SearchStats

trait HasChildren[S, A, V, R] {
  type Child <: BanditNode[S, A, V, R]

  def children: Array[Child]

  def searchState: SearchState

  def searchStats: SearchStats

  def uctExplorationCoefficient: V

  def costBound: Option[V]
}