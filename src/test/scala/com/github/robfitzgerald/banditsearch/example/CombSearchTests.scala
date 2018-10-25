package com.github.robfitzgerald.banditsearch.example

import com.github.robfitzgerald.DefaultTest

class CombSearchTests extends DefaultTest {
  "CombTest" when {
    "called with a 11-item vector problem" should {
      "be fun" in new CombSearch {
        def realSolution: State = Vector.fill(10)(0)
        def activatedPayloadLimit: Int = 5
        def totalPayloadCapacity: Int = 20

        runSearch(50, 1000L * 60 * 60, 1000) match {
          case None =>
            fail()
          case Some(result) =>
            println(result)
        }
      }
    }
  }
}
