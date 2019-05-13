package com.github.robfitzgerald.dabtree.local.randomselection

import com.github.robfitzgerald.dabtree.local.banditnode.{BanditChild, BanditParent}

import spire.random.rng.Cmwc5

object RandomSelection {

  def complimentaryMultiplyWithCarry[S, A, V]: BanditParent[S, A, V] => Int = {
    val random = Cmwc5()
    parent: BanditParent[S, A, V] => random.nextInt(parent.children.length)
  }

  def scalaRandom[S, A, V]: BanditParent[S, A, V] => Int = {
    val random = new scala.util.Random
    parent: BanditParent[S, A, V] => random.nextInt(parent.children.length)
  }

}