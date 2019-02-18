package com.github.robfitzgerald.dabtree.banditnode

trait BanditNode[S, A, V, R] {
  def state: S

  def action: Option[A]

  def reward: R
}