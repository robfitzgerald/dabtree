package com.github.robfitzgerald.dabtree.spark.example

import java.io.PrintWriter

import scala.util.Try
import scala.util.matching.Regex

import org.apache.spark.{SparkConf, SparkContext}
import cats.syntax.apply._

import com.github.robfitzgerald.dabtree.spark.sampler.pedrosorei
import com.github.robfitzgerald.dabtree.spark.searchcontext.DoublePrecisionCollect
import com.github.robfitzgerald.dabtree.spark.RankingDoublePrecision
import com.github.robfitzgerald.dabtree.spark.sampler.pedrosorei.Payload
import com.monovore.decline._

object CombinatorialSearchTrialRunner extends CommandApp(
  name = "Combinatorial Search Trial Runner",
  header = "DABTree applied to a combinatorial search solving function approximation",
  main = {

//    val ctxOption = Opts.option[String]("ctx", help = "Spark Context master: 'spark-local-numCores-numPartitions' or 'spark-yarn-numCores-numPartitions'").withDefault("spark-local-3-1")
    val coresOption = Opts.option[Int]("cores", help = "spark cores").withDefault(1)
    val partitionsOption = Opts.option[Int]("partitions", help = "partitions per executor").withDefault(1)
    val scOption = Opts.option[String]("sc", help = "sample confidence").withDefault("25")
    val defaultActivePayloads: Int = 17
    val actOption = Opts.option[String]("act", help = "max number of active payloads per iteration").withDefault(defaultActivePayloads.toString)
    val payloadsOption = Opts.option[String]("pl", help = "max number of active + suspended payloads per iteration").withDefault((defaultActivePayloads * 2).toString)
    val pStarOption = Opts.option[String]("pStar", help = "threshold for child promotion").withDefault("0.5")
    val trialsOption = Opts.option[Int]("trials", help = "number of trials per configuration").withDefault(1)
    val probSizeOption = Opts.option[Int]("pSize", help = "number of dimensions of the problem space").withDefault(20)

    (coresOption, partitionsOption, scOption, actOption, payloadsOption, pStarOption, trialsOption, probSizeOption).mapN { (cores, partitions, sc, act, pl, pStar, trials, probSize) =>
      println(s"spark $cores cores, $partitions partitions, run with args: --sc=$sc --act=$act --pl=$pl --trials=$trials --pSize=$probSize")
      val sparkContext = new SparkContext(new SparkConf().setMaster("yarn").setAppName("DabtreeSpark-CombinatorialTest"))
      val scRange = CombSearchExperiment.parseIntegers(sc)
      val actRange = CombSearchExperiment.parseIntegers(act)
      val plRange = CombSearchExperiment.parseIntegers(pl)
      val pStarRange = CombSearchExperiment.parseDecimals(pStar)
      new CombSearchExperiment(sparkContext, cores, partitions, scRange, actRange, plRange, pStarRange, trials, probSize).run()
    }
  }
)

class CombSearchExperiment (
  initSparkContext: SparkContext,
  cores: Int,
  partitions  : Int,
  scRange     : Seq[Int],
  actRange    : Seq[Int],
  plRange     : Seq[Int],
  pStarRange  : Seq[Double],
  numTrials   : Int,
  problemDimensionality: Int
) {

  case class Stats(
    sumCost: Double = 0.0,
    sumPayloads: Int = 0,
    sumAct: Int = 0,
    sumSus: Int = 0,
    sumCan: Int = 0,
    sumSamples: Int = 0,
    sumIterations: Int = 0
  )

  val rawFileName: String = s"${System.currentTimeMillis}-combsearch-raw.csv"
  val rawFileHeader: String =
    "id,problemSize,numChildren,activatedPayloadLimit,totalPayloadCapacity,rankingPolicyName,maxIterations," +
      "sampleConfidence,samplesPerIteration,maxDurationSeconds,trial" +
      DoublePrecisionCollect.csvHeader +
      "\n"
  val rawFileOutput: PrintWriter = new PrintWriter(rawFileName)
  rawFileOutput.append(rawFileHeader)
  val aggFileName = s"${System.currentTimeMillis}-combsearch-agg-${numTrials}trials.csv"
  val aggFileHeader = "label,avgCost,avgPayloads,act,sus,can,avgSamples,avgIterations\n"
  val aggFileOutput: PrintWriter = new PrintWriter(aggFileName)
  aggFileOutput.append(aggFileHeader)
  print(aggFileHeader)

  var counter: Int = 1

  def run(): Unit = {
    for {
      rankingPolicyParam <- Seq(
        ("costbound", RankingDoublePrecision.CostLowerBoundedRanking[Vector[Double], Double])
        //      ("reward", Ranking.GenericRanking[Vector[Double], Double, Double]),
        //      ("treedepth", CombSearch.TreeDepthRankingPolicy(problemSizeParam)),
        //      ("clbreward", Ranking.LowerBoundedAndRewardRanking[Vector[Double], Double, Double])
        //      ("clbtreedepth", CombSearch.CostBoundAndTreeDepthPolicy(problemSizeParam)),
        //      ("clb*treedepth", CombSearch.CostBoundTimesTreeDepthPolicy(problemSizeParam))
      )
      sc <- scRange
      act <- actRange
      pLimit <- plRange
      pStar <- pStarRange
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

        //        ctx match {
        //          case ExecutionContext.Local =>
        //            throw new IllegalArgumentException("please run using Spark")
        //          case ExecutionContext.Spark(sparkConf, cores, partitions) =>
        new SparkCombSearchRunner {

          def sparkContext: SparkContext = initSparkContext

          def parallelism: Int = cores * partitions

          def minValue: Value = 0.0

          def maxValue: Value = Double.MaxValue

          def numChildren: Int = numChildrenParam

          def problemSize: Int = problemDimensionality

          def rankingPolicy: Payload[Vector[Double], Double, Double] => Double = rankingPolicyFn

          def activatedPayloadLimit: Int = act

          def totalPayloadCapacity: Int = pLimit

          override def pStarPromotion: Double = pStar

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
                    sumSamples = stats.sumSamples + result.samples,
                    sumIterations = stats.sumIterations + result.iterations
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
        sumSamples = stats.sumSamples / numTrials,
        sumIterations = stats.sumIterations / numTrials
      )


      val label = s"pLim=$pLimit-actPl=$act-sc=$sc-pStar=$pStar"
      val aggRow = s"$label,${a.sumCost},${a.sumPayloads},${a.sumAct},${a.sumSus},${a.sumCan},${a.sumSamples},${a.sumIterations}\n"
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
    case class Spark (sparkContext: SparkConf, cores: Int, partitions: Int) extends ExecutionContext

    val SparkRegex: Regex = "spark-(\\w+)-(\\d+)-(\\d+)".r

    def apply(str: String): ExecutionContext = str match {
      case "local" => Local
      case SparkRegex(context, coresStr, partitionsStr) =>
        Try {
          val cores = coresStr.toInt
          val partitions = partitionsStr.toInt
          val sparkMaster =
            if (context == "yarn") context
            else if (context == "local") s"local[$cores]"
            else throw new IllegalArgumentException(s"Unable to parse context (local, yarn) of ctx parameter $str")
          val sparkConf = new SparkConf().setMaster(sparkMaster).setAppName("CombSearchApp")
          Spark(sparkConf, cores, partitions)
        } match {
          case util.Failure(e) => throw new IllegalArgumentException(s"Failed to parse ctx parameter.\n$e")
          case util.Success(sc) => sc
        }

      case _ => throw new IllegalArgumentException(s"invalid execution context $str")
    }
  }

  def parseDecimals(str: String): Seq[Double] = {
    Try {
      str.toDouble
    } match {
      case util.Success(n) => Seq(n)
      case util.Failure(_) =>
        parseDoubleList(str) match {
          case Some(intList) => intList
          case None => throw new IllegalArgumentException(s"could not parse argument $str as decimal")
        }
    }
  }

  def parseIntegers(str: String): Seq[Int] = {
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

  def parseDoubleList(numList: String): Option[Seq[Double]] = Try {
    numList.split(",").map{ _.toDouble }
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

