package com.github.robfitzgerald.banditsearch.sampler.pedrosorei

case class UCBPedrosoReiSamplerState [S, A, V](gBest: V, gWorst: V, Cp: Double, bestSolution: Option[S] = None, bestAction: Option[A] = None)
