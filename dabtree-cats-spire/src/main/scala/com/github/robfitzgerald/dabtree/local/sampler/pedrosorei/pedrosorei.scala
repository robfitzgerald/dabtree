package com.github.robfitzgerald.dabtree.local.sampler

import com.github.robfitzgerald.dabtree.local.banditnode.BanditParent

package object pedrosorei {
  type Payload[S,A,V] = (BanditParent[S, A, V], Option[UCBPedrosoReiGlobals[S, A, V]])
}
