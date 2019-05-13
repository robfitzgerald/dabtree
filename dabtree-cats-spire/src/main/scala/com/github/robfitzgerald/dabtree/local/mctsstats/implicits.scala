package com.github.robfitzgerald.dabtree.local.mctsstats

import com.github.robfitzgerald.dabtree.local.mctsstats.immutable.MCTSStatsImmutableTypeclass
import com.github.robfitzgerald.dabtree.local.mctsstats.mutable.MCTSStatsMutableTypeclass

object implicits
  extends MCTSStatsTypeclass
  with MCTSStatsImmutableTypeclass
  with MCTSStatsMutableTypeclass {

}
