package com.github.robfitzgerald.dabtree.spark.example

import java.io.PrintWriter

import scala.annotation.tailrec
import scala.util.Try
import scala.util.matching.Regex

import org.apache.spark.{SparkConf, SparkContext}
import cats.syntax.apply._

import com.github.robfitzgerald.dabtree.spark.RankingDoublePrecision
import com.github.robfitzgerald.dabtree.spark.sampler.pedrosorei.Payload
import com.github.robfitzgerald.dabtree.spark.searchcontext.DoublePrecisionCollect
import com.monovore.decline._

object CombinatorialSearchTrialRunner extends CommandApp(
  name = "Combinatorial Search Trial Runner",
  header = "DABTree applied to a combinatorial search solving function approximation",
  main = {
    val ctxOption = Opts.option[String]("ctx", help = "Spark Context master such as 'local[*]' or 'yarn'").withDefault("local[*]")
    val coresOption = Opts.option[Int]("cores", help = "spark cores").withDefault(1)
    val partitionsOption = Opts.option[Int]("partitions", help = "partitions per executor").withDefault(1)
    val scOption = Opts.option[String]("sc", help = "sample confidence").withDefault("25")
    val defaultActivePayloads: Int = 17
    val actOption = Opts.option[String]("act", help = "max number of active payloads per iteration").withDefault(defaultActivePayloads.toString)
    val payloadsOption = Opts.option[String]("pl", help = "max number of active + suspended payloads per iteration").withDefault((defaultActivePayloads * 2).toString)
    val pStarOption = Opts.option[String]("pStar", help = "threshold for child promotion").withDefault("0.5")
    val trialsOption = Opts.option[Int]("trials", help = "number of trials per configuration").withDefault(1)
    val durOption = Opts.option[Int]("dur", help = "trial time limit, aka computational budget, in seconds").withDefault(5)
    val probSizeOption = Opts.option[Int]("pSize", help = "number of dimensions of the problem space").withDefault(20)

    (ctxOption, coresOption, partitionsOption, scOption, actOption, payloadsOption, pStarOption, trialsOption, durOption, probSizeOption).mapN { (ctx, cores, partitions, sc, act, pl, pStar, trials, dur, probSize) =>
      println(s"spark $cores cores, $partitions partitions, run with args: --sc=$sc --act=$act --pl=$pl --trials=$trials --pSize=$probSize")
      val sparkContext = new SparkContext(new SparkConf().setMaster(ctx).setAppName("DabtreeSpark-CombinatorialTest"))
      sparkContext.setLogLevel("WARN")
      val scRange = CombSearchExperiment.parseIntegers(sc)
      val actRange = CombSearchExperiment.parseIntegers(act)
      val plRange = CombSearchExperiment.parseIntegers(pl)
      val pStarRange = CombSearchExperiment.parseDecimals(pStar)
      new CombSearchExperiment(sparkContext, cores, partitions, scRange, actRange, plRange, pStarRange, trials, dur, probSize).run()
    }
  }
)

class CombSearchExperiment (
  initSparkContext: SparkContext,
  coresArg: Int,
  partitionsArg  : Int,
  scRange     : Seq[Int],
  actRange    : Seq[Int],
  plRange     : Seq[Int],
  pStarRange  : Seq[Double],
  trials   : Int,
  dur  : Int,
  problemDimensionality: Int
) {

  val rawFileName: String = s"${System.currentTimeMillis}-combsearch-raw.csv"
  val rawFileHeader: String =
    "id,problemSize,numChildren,activatedPayloadLimit,totalPayloadCapacity,rankingPolicyName,maxIterations," +
      "sampleConfidence,samplesPerIteration,maxDurationSeconds,trial" +
      DoublePrecisionCollect.csvHeader +
      "\n"
  val rawFileOutput: PrintWriter = new PrintWriter(rawFileName)
  rawFileOutput.append(rawFileHeader)
  val aggFileName = s"${System.currentTimeMillis}-combsearch-agg-${trials}trials.csv"
  val aggFileHeader = "label,avgCost,avgPayloads,act,sus,can,avgSamples,avgIterations\n"
  val aggFileOutput: PrintWriter = new PrintWriter(aggFileName)
  aggFileOutput.append(aggFileHeader)
  print(aggFileHeader)

  val outputHeaders: (List[String], List[String]) = (List(rawFileHeader), List(aggFileHeader))

  var counter: Int = 1

  def run(): Unit = {
    val experiments: Seq[CombSearchExperiment.Experiment] =
      for {
        (rankingPolicyName, rankingPolicyFunction) <- Seq(
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
        maxIters = 50
        maxDurationSeconds = dur
        maxDur = maxDurationSeconds * 1000L
      } yield {

        val samplesPerIter = sc * numChildrenParam
        CombSearchExperiment.Experiment(
          maxIters,
          maxDur,
          sc,
          samplesPerIter,
          0.0,
          Double.MaxValue,
          numChildrenParam,
          problemDimensionality,
          rankingPolicyName,
          rankingPolicyFunction,
          act,
          pLimit,
          pStar
        )
      }

    val (rawLines, aggLines) = CombSearchExperiment.runExperimentalConfig(
      initSparkContext,
      coresArg,
      partitionsArg,
      experiments,
      trials,
      (List.empty, List.empty)
    )

    for {
      line <- rawLines
    } {
      rawFileOutput.write(line)
    }

    for {
      line <- aggLines
    } {
      aggFileOutput.write(line)
    }

    rawFileOutput.close()
    aggFileOutput.close()
  }
}

object CombSearchExperiment {

  case class Experiment(
    maxIterations: Int,
    maxDuration: Long,
    sampleConfidence: Int,
    samplesPerIteration: Int,
    minValue: Double,
    maxValue: Double,
    numChildren: Int,
    problemSize: Int,
    rankingPolicyName: String,
    rankingPolicy: Payload[Vector[Double], Double, Double] => Double,
    activatedPayloadLimit: Int,
    totalPayloadCapacity: Int,
    pStarPromotion: Double
  )

  case class Stats(
    sumCost: Double = 0.0,
    sumPayloads: Int = 0,
    sumAct: Int = 0,
    sumSus: Int = 0,
    sumCan: Int = 0,
    sumSamples: Long = 0,
    sumIterations: Int = 0
  )

  @tailrec
  def runExperimentalConfig(
    initSparkContext: SparkContext,
    coresArg: Int,
    partitionsArg: Int,
    experiments: Seq[Experiment], 
    trials: Int,
    output: (List[String], List[String]) = (List.empty[String], List.empty[String])
  ): (List[String], List[String]) = {
    if (experiments.isEmpty) output
    else {
      val exp = experiments.head
      
      var trial = 1
      var statsAccumulator = Stats()
      val thisOutput = collection.mutable.ListBuffer.empty[String]
      
      do {
        val search: SparkCombSearchRunner = new SparkCombSearchRunner {

          val maxIterations: Int = exp.maxIterations
          val maxDuration: Long = exp.maxDuration
          val samplesPerIteration: Int = exp.samplesPerIteration
          def sparkContext: SparkContext = initSparkContext

          def workingDirectory: String = "/tmp"
          def cores: Int = coresArg
          def partitions: Int = partitionsArg
          def minValue: Value = exp.minValue
          def maxValue: Value = exp.maxValue
          def numChildren: Int = exp.numChildren
          def problemSize: Int = exp.problemSize
          def rankingPolicy: Payload[Vector[Double], Double, Double] => Double = exp.rankingPolicy
          def activatedPayloadLimit: Int = exp.activatedPayloadLimit
          def totalPayloadCapacity: Int = exp.totalPayloadCapacity
          override def pStarPromotion: Double = exp.pStarPromotion
        }
        
        
        search.runSearch(exp.maxIterations, exp.maxDuration, exp.samplesPerIteration) match {
          case None =>
          case Some(result) =>
            result.bestCost match {
              case None =>
                thisOutput.append(s"$trial,${exp.problemSize},${exp.numChildren},${exp.activatedPayloadLimit},${exp.totalPayloadCapacity}," +
                  s"${exp.rankingPolicyName},${exp.maxIterations},${exp.sampleConfidence},${exp.samplesPerIteration},${exp.maxDuration},$trial," +
                  ",,,,,\n")
              case Some(bestCost) =>
                statsAccumulator = statsAccumulator.copy(
                  sumCost = statsAccumulator.sumCost + bestCost,
                  sumPayloads = statsAccumulator.sumPayloads + result.payloadsCount,
                  sumAct = statsAccumulator.sumAct + result.activatedCount,
                  sumSus = statsAccumulator.sumSus + result.suspendedCount,
                  sumCan = statsAccumulator.sumCan + result.cancelledCount,
                  sumSamples = statsAccumulator.sumSamples + result.samples,
                  sumIterations = statsAccumulator.sumIterations + result.iterations
                )
                thisOutput.append(s"$trial,${exp.problemSize},${exp.numChildren},${exp.activatedPayloadLimit},${exp.totalPayloadCapacity}," +
                  s"${exp.rankingPolicyName},${exp.maxIterations},${exp.sampleConfidence},${exp.samplesPerIteration},${exp.maxDuration},$trial," +
                  result.toCSVString +
                  "\n")
            }
        }
        
        trial += 1
        
      } while (trial <= trials)

      // summarize
      val a = statsAccumulator.copy(
        sumCost = statsAccumulator.sumCost / trials,
        sumPayloads = statsAccumulator.sumPayloads / trials,
        sumAct = statsAccumulator.sumAct / trials,
        sumSus = statsAccumulator.sumSus / trials,
        sumCan = statsAccumulator.sumCan / trials,
        sumSamples = statsAccumulator.sumSamples / trials,
        sumIterations = statsAccumulator.sumIterations / trials
      )


      val label = s"pLim=${exp.totalPayloadCapacity}-actPl=${exp.activatedPayloadLimit}-sc=${exp.sampleConfidence}-pStar=${exp.pStarPromotion}"
      val aggRow = s"$label,${a.sumCost},${a.sumPayloads},${a.sumAct},${a.sumSus},${a.sumCan},${a.sumSamples},${a.sumIterations}\n"
      print(aggRow)

      val (raw, agg) = output
      val updatedOutput = (raw ::: thisOutput.toList, agg :+ aggRow)

      runExperimentalConfig(
        initSparkContext,
        coresArg,
        partitionsArg,
        experiments.tail,
        trials,
        updatedOutput
      )
    }
  }

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

