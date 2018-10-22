package com.github.robfitzgerald.banditsearch.mctsstats

import com.github.robfitzgerald.banditsearch.mctsstats.immutable.MCTSStatsImmutableTypeclass
import com.github.robfitzgerald.banditsearch.mctsstats.mutable.MCTSStatsMutableTypeclass

object implicits extends MCTSStatsTypeclass
  with MCTSStatsImmutableTypeclass
  with MCTSStatsMutableTypeclass {}
