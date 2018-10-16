package com.github.robfitzgerald.banditsearch.banditnode

import com.github.robfitzgerald.banditsearch.MCTSStats

trait HasMCTSStats [V] {
  def mctsStats: MCTSStats[V]
}