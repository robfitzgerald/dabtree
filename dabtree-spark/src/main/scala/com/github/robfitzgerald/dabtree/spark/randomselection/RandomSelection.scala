package com.github.robfitzgerald.dabtree.spark.randomselection

import com.github.robfitzgerald.dabtree.spark.banditnode.{SparkBanditChild, SparkBanditParent}

object RandomSelection {

  def scalaRandom[S, A]: SparkBanditParent[S, A] => Int = {
    val random = new scala.util.Random
    parent: SparkBanditParent[S, A] => random.nextInt(parent.children.length)
  }

}