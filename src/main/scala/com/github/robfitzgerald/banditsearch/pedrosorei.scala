package com.github.robfitzgerald.banditsearch

import com.github.robfitzgerald.banditsearch.banditnode.BanditParent
import com.github.robfitzgerald.banditsearch.sampler.pedrosorei.UCBPedrosoReiGlobals

object pedrosorei {
  type Payload[S,A,V] = (BanditParent[S, A, V], Option[UCBPedrosoReiGlobals[S, A, V]])
}
