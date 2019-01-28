package ru.ifmo.iorthant.jmh

import java.util.Random
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import ru.ifmo.iorthant.noq2d.{NoUpdateIncrementalOrthantSearch, PlainArray, SimpleKD}
import ru.ifmo.iorthant.util.{HasMinus, LiveDeadSet, Monoid}
import ru.ifmo.iorthant.util.Syntax._

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Timeout(time = 1, timeUnit = TimeUnit.HOURS)
@Warmup(iterations = 1, time = 6)
@Measurement(iterations = 1, time = 1)
@Fork(value = 5)
class NoUpdateBenchmark {
  import NoUpdateBenchmark._

  //noinspection VarCouldBeVal: this inspection shall be suppressed for everything @Param
  @Param(Array("10", "31", "100", "316", "1000"))
  private var n: Int = _

  //noinspection VarCouldBeVal: this inspection shall be suppressed for everything @Param
  @Param(Array("2", "3", "4", "5"))
  private var d: Int = _

  //noinspection VarCouldBeVal: this inspection shall be suppressed for everything @Param
  @Param(Array("plain", "kd-simple"))
  private var algorithm: String = _

  //noinspection VarCouldBeVal: this inspection shall be suppressed for everything @Param
  @Param(Array("plane", "cube", "line"))
  private var test: String = _

  private var instances: Array[Array[Action]] = _

  private def generatePoint(rng: Random): Array[Double] = test match {
    case "cube" =>
      Array.fill(d)(rng.nextDouble())
    case "line" =>
      val v = rng.nextDouble()
      Array.fill(d)(v)
    case "plane" =>
      Array.fill(d)(rng.nextDouble()).whereAlso(a => a(0) += 1.0 - a.sum)
  }

  @Setup
  def initialize(): Unit = instances = Array.tabulate(10) { i =>
    // intentionally do not depend on "algorithm"
    val rng = new Random(i * 72433566236111L + n * 623432 + d * 91274635553235L + test.hashCode)
    val queryIndices, dataIndices = new LiveDeadSet(n)
    val actions = Array.newBuilder[Action]
    var wasQueryFull, wasDataFull = false
    for (_ <- 0 until 10 * n) {
      if (rng.nextBoolean()) {
        if (rng.nextDouble() < (1 - math.pow(2, queryIndices.nLive - n)) / (1 - math.pow(2, -n))) {
          // add a query
          actions += new AddQuery(generatePoint(rng), queryIndices.reviveRandom(rng))
        } else {
          // delete a query
          actions += new RemoveQuery(queryIndices.killRandom(rng))
        }
      } else {
        if (rng.nextDouble() < (1 - math.pow(2, dataIndices.nLive - n)) / (1 - math.pow(2, -n))) {
          // add a query
          actions += new AddData(generatePoint(rng), rng.nextDouble(), dataIndices.reviveRandom(rng))
        } else {
          // delete a query
          actions += new RemoveData(dataIndices.killRandom(rng))
        }
      }
      wasDataFull |= dataIndices.nDead == 0
      wasQueryFull |= queryIndices.nDead == 0
    }
    assert(wasDataFull && wasQueryFull, s"$n: $wasDataFull, $wasQueryFull")
    actions.result()
  }

  @Benchmark
  def benchmark(bh: Blackhole): Unit = {
    for (actions <- instances) {
      val w = new AlgorithmWrapper(algorithm, n, n)
      for (action <- actions) {
        action.perform(w)
      }
    }
  }
}

object NoUpdateBenchmark {
  class AlgorithmWrapper(algorithmName: String, nDataPoints: Int, nQueryPoints: Int) {
    final val algorithm: NoUpdateIncrementalOrthantSearch[Double] = algorithmName match {
      case "plain"             => new PlainArray[Double]()(TestMonoid)
      case "kd-simple"         => new SimpleKD[Double](0)(TestMonoid)
    }
    final val dataPoints = algorithm.newDataPointHandleArray(nDataPoints)
    final val queryPoints = algorithm.newQueryPointHandleArray(nDataPoints)
  }

  object IgnoreTracker extends NoUpdateIncrementalOrthantSearch.UpdateTracker[Double, AnyRef] {
    override def valueChanged(value: Double, identifier: AnyRef): Unit = {}
  }

  implicit object TestMonoid extends Monoid[Double] with HasMinus[Double] {
    override def zero: Double = 0
    override def plus(lhs: Double, rhs: Double): Double = lhs + rhs
    override def minus(lhs: Double, rhs: Double): Double = lhs - rhs
  }

  abstract class Action {
    def perform(w: AlgorithmWrapper): Unit
  }

  class AddQuery(point: Array[Double], index: Int) extends Action {
    override def perform(w: AlgorithmWrapper): Unit = {
      w.queryPoints(index) = w.algorithm.addQueryPoint(point, IgnoreTracker, IgnoreTracker)
    }
  }

  class AddData(point: Array[Double], value: Double, index: Int) extends Action {
    override def perform(w: AlgorithmWrapper): Unit = {
      w.dataPoints(index) = w.algorithm.addDataPoint(point, value)
    }
  }

  class RemoveQuery(index: Int) extends Action {
    override def perform(w: AlgorithmWrapper): Unit = {
      w.algorithm.removeQueryPoint(w.queryPoints(index))
    }
  }

  class RemoveData(index: Int) extends Action {
    override def perform(w: AlgorithmWrapper): Unit = {
      w.algorithm.removeDataPoint(w.dataPoints(index))
    }
  }
}
