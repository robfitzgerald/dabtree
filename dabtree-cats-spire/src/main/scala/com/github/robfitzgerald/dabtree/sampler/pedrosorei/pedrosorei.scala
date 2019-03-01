package com.github.robfitzgerald.dabtree.sampler

import com.github.robfitzgerald.dabtree.banditnode.BanditParent

package object pedrosorei {
  type Payload[S,A,V] = (BanditParent[S, A, V], Option[UCBPedrosoReiGlobals[S, A, V]])
}
