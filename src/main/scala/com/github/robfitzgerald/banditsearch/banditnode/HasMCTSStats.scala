package com.github.robfitzgerald.banditsearch.banditnode

import com.github.robfitzgerald.banditsearch.mctsstats.immutable.MCTSStatsImmutableImpl

trait HasMCTSStats [F[_], V] {
  def mctsStats: MCTSStatsImmutableImpl[F, V]
}