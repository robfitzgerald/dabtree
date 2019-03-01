package com.github.robfitzgerald.dabtree.common.banditnode

sealed trait SearchState {}

object SearchState {
  case object Activated extends SearchState
  case object Suspended extends SearchState
  case object Cancelled extends SearchState
}