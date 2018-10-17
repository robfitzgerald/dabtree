package com.github.robfitzgerald.banditsearch.randomselection

import com.github.robfitzgerald.banditsearch.banditnode.{BanditChild, BanditParent}

import spire.random.rng.Cmwc5

object RandomSelection {

  def complimentaryMultiplyWithCarry[S, A, V]: BanditParent[S, A, V] => BanditChild[S, A, V] = {
    val random = Cmwc5()
    parent: BanditParent[S, A, V] => parent.children(random.nextInt(parent.children.length))
  }

}