package com.github.robfitzgerald.banditsearch.banditnode

trait BanditNode[F[_], S, A, V, R] extends HasMCTSStats[F, V] {
  def state: S

  def action: Option[A]

  def reward: R
}

