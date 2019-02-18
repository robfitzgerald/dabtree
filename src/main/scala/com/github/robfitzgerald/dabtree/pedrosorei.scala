package com.github.robfitzgerald.dabtree

import com.github.robfitzgerald.dabtree.banditnode.BanditParent
import com.github.robfitzgerald.dabtree.sampler.pedrosorei.UCBPedrosoReiGlobals

object pedrosorei {
  type Payload[S,A,V] = (BanditParent[S, A, V], Option[UCBPedrosoReiGlobals[S, A, V]])
}
