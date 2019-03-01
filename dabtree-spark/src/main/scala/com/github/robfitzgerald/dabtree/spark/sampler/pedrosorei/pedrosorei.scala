package com.github.robfitzgerald.dabtree.spark.sampler

import com.github.robfitzgerald.dabtree.spark.banditnode.SparkBanditParent

package object pedrosorei {
  type Payload[S,A,V] = (SparkBanditParent[S, A], Option[UCBPedrosoReiGlobals[S, A, V]])
}
