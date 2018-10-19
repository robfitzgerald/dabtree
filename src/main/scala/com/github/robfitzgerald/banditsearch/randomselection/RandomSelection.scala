package com.github.robfitzgerald.banditsearch.randomselection

import com.github.robfitzgerald.banditsearch.banditnode.{BanditChild, BanditParent}

import spire.random.rng.Cmwc5

object RandomSelection {

  def complimentaryMultiplyWithCarry[F[_], S, A, V]: BanditParent[F, S, A, V] => BanditChild[F, S, A, V] = {
    val random = Cmwc5()
    parent: BanditParent[F, S, A, V] => parent.children(random.nextInt(parent.children.length))
  }

}