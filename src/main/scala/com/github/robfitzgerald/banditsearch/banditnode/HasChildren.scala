package com.github.robfitzgerald.banditsearch.banditnode

import com.github.robfitzgerald.banditsearch.SearchStats

trait HasChildren[F[_], S, A, V, R] {
  type Child <: BanditNode[F, S, A, V, R]

  def children: Array[Child]

  def searchState: SearchState

  def searchStats: SearchStats

  def uctExplorationCoefficient: V

  def costBound: Option[V]
}