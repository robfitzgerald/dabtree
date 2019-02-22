package com.github.robfitzgerald.dabtree.example

import java.io.PrintWriter

import scala.util.Try

import cats.implicits._

import com.github.robfitzgerald.dabtree.Ranking
import com.github.robfitzgerald.dabtree.pedrosorei.Payload
import com.github.robfitzgerald.dabtree.searchcontext.GenericPedrosoReiCollect
import com.monovore.decline._

object CombinatorialSearchTrialRunner extends CommandApp(
  name = "Combinatorial Search Trial Runner",
  header = "DABTree applied to a combinatorial search solving function approximation",
  main = {

    val scOption = Opts.option[String]("sc", help = "sample confidence").withDefault("25")
    val defaultActivePayloads: Int = 17
    val actOption = Opts.option[String]("act", help = "max number of active payloads per iteration").withDefault(defaultActivePayloads.toString)
    val payloadsOption = Opts.option[String]("pl", help = "max number of active + suspended payloads per iteration").withDefault((defaultActivePayloads * 2).toString)
    val trialsOption = Opts.option[Int]("trials", help = "number of trials per configuration").withDefault(1)
    val probSizeOption = Opts.option[Int]("pSize", help = "number of dimensions of the problem space").withDefault(20)

    (scOption, actOption, payloadsOption, trialsOption, probSizeOption).mapN { (sc, act, pl, trials, probSize) =>
      println(s"run with args: --sc=$sc --act=$act --pl=$pl --trials=$trials --pSize=$probSize")
      val scRange = CombinatorialSearchTrialRunner.parseIntList(sc)
      val actRange = CombinatorialSearchTrialRunner.parseIntList(act)
      val plRange = CombinatorialSearchTrialRunner.parseIntList(pl)
      new CombSearchExperiment(scRange, actRange, plRange, trials, probSize).run()
    }
  }
) {
  def parseIntList(numList: String): Seq[Int] = Try {
    numList.split(",").map{ _.toInt }
  } match {
    case util.Success(asNumeric) => asNumeric
    case util.Failure(e) =>
      println(s"couldn't parse integers from $numList")
      Seq.empty[Int]
  }
}

class CombSearchExperiment (scRange: Seq[Int], actRange: Seq[Int], plRange: Seq[Int], numTrials: Int, problemDimensionality: Int) {

  case class Stats(
    sumCost: Double = 0.0,
    sumPayloads: Int = 0,
    sumAct: Int = 0,
    sumSus: Int = 0,
    sumCan: Int = 0,
    sumSamples: Int = 0
  )

  val rawFileName: String = s"${System.currentTimeMillis}-combsearch-raw.csv"
  val rawFileHeader: String =
    "id,problemSize,numChildren,activatedPayloadLimit,totalPayloadCapacity,rankingPolicyName,maxIterations," +
      "sampleConfidence,samplesPerIteration,maxDurationSeconds,trial," +
      GenericPedrosoReiCollect.csvHeader +
      "\n"
  val rawFileOutput: PrintWriter = new PrintWriter(rawFileName)
  rawFileOutput.append(rawFileHeader)
  val aggFileName = s"${System.currentTimeMillis}-combsearch-agg-${numTrials}trials.csv"
  val aggFileHeader = "label,avgCost,avgPayloads,act,sus,can,avgSamples\n"
  val aggFileOutput: PrintWriter = new PrintWriter(aggFileName)
  aggFileOutput.append(aggFileHeader)
  print(aggFileHeader)

  var counter: Int = 1

  def run(): Unit = {
    for {
      rankingPolicyParam  <- Seq(
        ("costbound", Ranking.CostLowerBoundedRanking[Vector[Double], Double, Double])
        //      ("reward", Ranking.GenericRanking[Vector[Double], Double, Double]),
        //      ("treedepth", CombSearch.TreeDepthRankingPolicy(problemSizeParam)),
        //      ("clbreward", Ranking.LowerBoundedAndRewardRanking[Vector[Double], Double, Double])
        //      ("clbtreedepth", CombSearch.CostBoundAndTreeDepthPolicy(problemSizeParam)),
        //      ("clb*treedepth", CombSearch.CostBoundTimesTreeDepthPolicy(problemSizeParam))
      )
      sc <- scRange
      act <- actRange
      pLimit <- plRange
      numChildrenParam = 11 // step by .25 or .1
      maxIterations = 100
      maxDurationSeconds = 5
      maxDuration = maxDurationSeconds * 1000L
    } {

      val samplesPerIteration = sc * numChildrenParam
      var stats: Stats = Stats()

      for {
        trial <- 1 to numTrials
      } yield {
        val (rankingPolicyName, rankingPolicyFn) = rankingPolicyParam

        new CombSearch {

          def minValue: Value = realSolution.min

          def maxValue: Value = math.sqrt(realSolution.size.toDouble)

          def numChildren: Int = numChildrenParam

          def problemSize: Int = problemDimensionality

          def rankingPolicy: Payload[Vector[Double], Double, Double] => Double = rankingPolicyFn

          def activatedPayloadLimit: Int = act

          def totalPayloadCapacity: Int = pLimit

          {
            for {
              result <- runSearch(maxIterations, maxDuration, sc * numChildrenParam)
            } yield {
              result.bestCost match {
                case None =>
                case Some(bestCost) =>
                  stats = stats.copy(
                    sumCost = stats.sumCost + bestCost,
                    sumPayloads = stats.sumPayloads + result.payloadsCount,
                    sumAct = stats.sumAct + result.activatedCount,
                    sumSus = stats.sumSus + result.suspendedCount,
                    sumCan = stats.sumCan + result.cancelledCount,
                    sumSamples = stats.sumSamples + result.samples
                  )
              }
              s"$counter,$problemDimensionality,$numChildrenParam,$act,$pLimit," +
                s"$rankingPolicyName,$maxIterations,$sc,$samplesPerIteration,$maxDurationSeconds,$trial," +
                result.toCSVString +
                "\n"
            }
          } match {
            case Some(row) =>
              //            print(row)
              rawFileOutput.append(row)
            case None =>
              val emptyRow: String =
                s"$counter,$problemDimensionality,$numChildrenParam,$act,$pLimit," +
                  s"$rankingPolicyName,$maxIterations,$sc,$samplesPerIteration,$maxDurationSeconds,$trial," +
                  ",,,,,\n"
              //            print(emptyRow)
              rawFileOutput.append(emptyRow)
          }
          counter += 1
        }
      }

      // summarize
      val a = stats.copy(
        sumCost = stats.sumCost / numTrials,
        sumPayloads = stats.sumPayloads / numTrials,
        sumAct = stats.sumAct / numTrials,
        sumSus = stats.sumSus / numTrials,
        sumCan = stats.sumCan / numTrials,
        sumSamples = stats.sumSamples / numTrials
      )

      val label = s"actPl=$act-ranking=${rankingPolicyParam._1}-sc=$sc"
      val aggRow = s"$label,${a.sumCost},${a.sumPayloads},${a.sumAct},${a.sumSus},${a.sumCan},${a.sumSamples}\n"
      print(aggRow)
      aggFileOutput.append(aggRow)
    }

    rawFileOutput.close()
    aggFileOutput.close()

    ()
  }
}


