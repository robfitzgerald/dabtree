package com.github.robfitzgerald.dabtree.example

import java.io.PrintWriter

import scala.util.Try
import scala.util.matching.Regex

import cats.implicits._

import com.github.robfitzgerald.dabtree.Ranking
import com.github.robfitzgerald.dabtree.pedrosorei.Payload
import com.github.robfitzgerald.dabtree.searchcontext.GenericPedrosoReiCollect
import com.monovore.decline._

object CombinatorialSearchTrialRunner extends CommandApp(
  name = "Combinatorial Search Trial Runner",
  header = "DABTree applied to a combinatorial search solving function approximation",
  main = {

    val ctxOption = Opts.option[String]("ctx", help = "execution context 'local', 'localpar'").withDefault("local")
    val scOption = Opts.option[String]("sc", help = "sample confidence").withDefault("25")
    val defaultActivePayloads: Int = 17
    val actOption = Opts.option[String]("act", help = "max number of active payloads per iteration").withDefault(defaultActivePayloads.toString)
    val payloadsOption = Opts.option[String]("pl", help = "max number of active + suspended payloads per iteration").withDefault((defaultActivePayloads * 2).toString)
    val trialsOption = Opts.option[Int]("trials", help = "number of trials per configuration").withDefault(1)
    val probSizeOption = Opts.option[Int]("pSize", help = "number of dimensions of the problem space").withDefault(20)

    (ctxOption, scOption, actOption, payloadsOption, trialsOption, probSizeOption).mapN { (ctxString, sc, act, pl, trials, probSize) =>
      println(s"run with args: --sc=$sc --act=$act --pl=$pl --trials=$trials --pSize=$probSize")
      val scRange = CombSearchExperiment.parseNumbers(sc)
      val actRange = CombSearchExperiment.parseNumbers(act)
      val plRange = CombSearchExperiment.parseNumbers(pl)
      val ctx = CombSearchExperiment.ExecutionContext(ctxString)
      new CombSearchExperiment(ctx, scRange, actRange, plRange, trials, probSize).run()
    }
  }
)

class CombSearchExperiment (ctx: CombSearchExperiment.ExecutionContext, scRange: Seq[Int], actRange: Seq[Int], plRange: Seq[Int], numTrials: Int, problemDimensionality: Int) {

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

object CombSearchExperiment {

  sealed trait ExecutionContext
  object ExecutionContext {
    case object Local extends ExecutionContext
    case object LocalPar extends ExecutionContext
    def apply(str: String): ExecutionContext = str match {
      case "local" => Local
      case "localpar" => LocalPar
      case _ => throw new IllegalArgumentException(s"invalid execution context $str")
    }
  }

  def parseNumbers(str: String): Seq[Int] = {
    Try {
      str.toInt
    } match {
      case util.Success(n) => Seq(n)
      case util.Failure(_) =>
        parseIntList(str) match {
          case Some(intList) => intList
          case None =>
            parseRange(str) match {
              case Some(range) => range
              case None => throw new IllegalArgumentException(s"could not parse argument $str as numeric")
            }
        }
    }
  }

  def parseIntList(numList: String): Option[Seq[Int]] = Try {
    numList.split(",").map{ _.toInt }
  } match {
    case util.Success(asNumeric) => Some { asNumeric }
    case util.Failure(_) => None
  }

  val RangeRegex: Regex = "(\\d+):(\\d+):(\\d+)".r

  def parseRange(range: String): Option[Seq[Int]] = Try {
    range match {
      case RangeRegex(start, end, step) =>
        start.toInt to end.toInt by step.toInt
    }
  } match {
    case util.Success(rangeInterpreted) => Some { rangeInterpreted }
    case util.Failure(_) => None
  }

}

