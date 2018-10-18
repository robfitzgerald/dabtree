package com.github.robfitzgerald.banditsearch.banditnode

import com.github.robfitzgerald.banditsearch.mctsstats.immutable.MCTSStatsImmutableImpl

trait HasMCTSStats [V] {
  def mctsStats: MCTSStatsImmutableImpl[V]
}