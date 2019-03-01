package com.github.robfitzgerald.dabtree.reward

import com.github.robfitzgerald.dabtree.DefaultTest
import spire.implicits._

class UCBPedrosoReiTests extends DefaultTest {
  "UCBPedrosoRei.rewardFunction" when {
    "edge cases" when {
      "gWorst == gBest" when {
        "called with 0 parent visits and 100 child visits (impossible)" should {
          "give a reward of 0" in {
            val result = UCBPedrosoRei.rewardFunction[Double](
              gBest = 1,
              gWorst = 1,
              lBest = 1,
              lAvg = 1,
              pVisits = 0,
              cVisits = 100,
              Cp = math.sqrt(2)
            )
            result should equal (0.0)
          }
        }
        "called with 0 parent visits and 0 child visits" should {
          "give a reward of infinity" in {
            val result = UCBPedrosoRei.rewardFunction[Double](
              gBest = 1,
              gWorst = 1,
              lBest = 1,
              lAvg = 1,
              pVisits = 0,
              cVisits = 0,
              Cp = math.sqrt(2)
            )

            result should equal (Double.PositiveInfinity)
          }
        }
        "called with 100 parent visits and 0 child visits" should {
          "give a reward of infinity" in {
            val result = UCBPedrosoRei.rewardFunction[Double](
              gBest = 1,
              gWorst = 1,
              lBest = 1,
              lAvg = 1,
              pVisits = 100,
              cVisits = 0,
              Cp = math.sqrt(2)
            )

            result should equal (Double.PositiveInfinity)
          }
        }
      }
    }
    "hand-wavey tests" when {
      "called with a low reward scenario and high reward scenario" should {
        "correctly be related" in {
          val lowReward = UCBPedrosoRei.rewardFunction[Double](
            gBest = 1,
            gWorst = 100,
            lBest = 60,
            lAvg = 80,
            pVisits = 100,
            cVisits = 50,
            Cp = math.sqrt(2)
          )
          val highReward = UCBPedrosoRei.rewardFunction[Double](
            gBest = 1,
            gWorst = 100,
            lBest = 10,
            lAvg = 55,
            pVisits = 100,
            cVisits = 50,
            Cp = math.sqrt(2)
          )

          lowReward should be < highReward
          highReward should be > 1.0
          lowReward should be < 0.5
        }
      }
    }
  }
}
