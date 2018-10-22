package com.github.robfitzgerald.banditsearch.banditnode

trait BanditNode[S, A, V, R] extends HasMCTSStats[V] {
  def state: S

  def action: Option[A]

  def reward: R
}