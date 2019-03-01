package com.github.robfitzgerald.dabtree.common.banditnode

import com.github.robfitzgerald.dabtree.common.SearchStats

trait HasChildren[S, A, V, R] {
  type Child <: BanditNode[S, A, V, R]

  def children: Array[Child]

  def searchState: SearchState

  def searchStats: SearchStats

  def costBound: Option[V]
}
