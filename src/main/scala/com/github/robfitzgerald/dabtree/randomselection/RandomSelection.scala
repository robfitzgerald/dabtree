package com.github.robfitzgerald.dabtree.randomselection

import com.github.robfitzgerald.dabtree.banditnode.{BanditChild, BanditParent}

import spire.random.rng.Cmwc5

object RandomSelection {

  def complimentaryMultiplyWithCarry[S, A, V]: BanditParent[S, A, V] => Int = {
    val random = Cmwc5()
    parent: BanditParent[S, A, V] => random.nextInt(parent.children.length)
  }

}