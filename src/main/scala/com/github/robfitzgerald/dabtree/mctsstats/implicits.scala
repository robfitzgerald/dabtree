package com.github.robfitzgerald.dabtree.mctsstats

import com.github.robfitzgerald.dabtree.mctsstats.immutable.MCTSStatsImmutableTypeclass
import com.github.robfitzgerald.dabtree.mctsstats.mutable.MCTSStatsMutableTypeclass

object implicits
  extends MCTSStatsTypeclass
  with MCTSStatsImmutableTypeclass
  with MCTSStatsMutableTypeclass {

}
