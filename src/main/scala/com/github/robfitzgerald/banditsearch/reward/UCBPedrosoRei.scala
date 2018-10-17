package com.github.robfitzgerald.banditsearch.reward

import com.github.robfitzgerald.banditsearch.banditnode.HasMCTSStats
import spire.math.Fractional

object UCBPedrosoRei {
  def apply[S, A, V: Fractional](node: HasMCTSStats[V], observation: V): Double = ???
}