package com.github.robfitzgerald.banditsearch.sampler

import com.github.robfitzgerald.banditsearch.sampler.pedrosorei.UCBPedrosoReiSamplerTypeclass
import com.github.robfitzgerald.banditsearch.sampler.pedrosorei.UCBPerosoReioGlobalStateOps

object implicits extends SamplerOps with UCBPedrosoReiSamplerTypeclass with UCBPerosoReioGlobalStateOps