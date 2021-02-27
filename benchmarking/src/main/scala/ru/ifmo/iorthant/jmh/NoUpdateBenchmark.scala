package ru.ifmo.iorthant.jmh

import java.util.Random
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

import ru.ifmo.iorthant.noq2d.{NoUpdateIncrementalOrthantSearch, PlainArray, SimpleKD}
import ru.ifmo.iorthant.util.{DataGenerator, HasNegation, LiveDeadSet, Monoid}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.SECONDS)
@Timeout(time = 1, timeUnit = TimeUnit.HOURS)
@Warmup(iterations = 1, time = 6)
@Measurement(iterations = 1, time = 1)
@Fork(value = 5)
class NoUpdateBenchmark {
  import NoUpdateBenchmark._

  //noinspection VarCouldBeVal: this inspection shall be suppressed for everything @Param
  @Param(Array("10", "31", "100", "316", "1000", "3162"))
  private var n: Int = _

  //noinspection VarCouldBeVal: this inspection shall be suppressed for everything @Param
  @Param(Array("2", "3", "4", "5", "7", "10"))
  private var d: Int = _

  //noinspection VarCouldBeVal: this inspection shall be suppressed for everything @Param
  @Param(Array("plain", "kd-simple"))
  private var algorithm: String = _

  //noinspection VarCouldBeVal: this inspection shall be suppressed for everything @Param
  @Param(Array("plane", "cube", "line"))
  private var test: String = _

  private var instances: Array[Array[Action]] = _

  @Setup
  def initialize(): Unit = instances = Array.tabulate(3) { i =>
    // intentionally do not depend on "algorithm"
    val rng = new Random(i * 72433566236111L + n * 623432 + d * 91274635553235L + test.hashCode)
    val queryIndices, dataIndices = new LiveDeadSet(n)
    val actions = Array.newBuilder[Action]
    var wasQueryFull, wasDataFull = false
    val generator = DataGenerator.lookup(test)
    for (_ <- 0 until 4 * n) {
      if (rng.nextBoolean()) {
        if (rng.nextDouble() < (1 - math.pow(2, queryIndices.nLive - n)) / (1 - math.pow(2, -n))) {
          // add a query
          actions += new AddQuery(generator.generate(rng, d), queryIndices.reviveRandom(rng))
        } else {
          // delete a query
          actions += new RemoveQuery(queryIndices.killRandom(rng))
        }
      } else {
        if (rng.nextDouble() < (1 - math.pow(2, dataIndices.nLive - n)) / (1 - math.pow(2, -n))) {
          // add a query
          actions += new AddData(generator.generate(rng, d), rng.nextDouble(), dataIndices.reviveRandom(rng))
        } else {
          // delete a query
          actions += new RemoveData(dataIndices.killRandom(rng))
        }
      }
      wasDataFull |= dataIndices.nDead == 0
      wasQueryFull |= queryIndices.nDead == 0
    }
    assert(n < 100 || wasDataFull && wasQueryFull, s"$n: $wasDataFull, $wasQueryFull")
    actions.result()
  }

  @OperationsPerInvocation(3)
  @Benchmark
  def benchmark(): Unit = {
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
    final val queryPoints = algorithm.newQueryPointHandleArray(nQueryPoints)
  }

  object IgnoreTracker extends NoUpdateIncrementalOrthantSearch.UpdateTracker[Double, AnyRef] {
    override def valueChanged(delta: Double, identifier: AnyRef): Unit = {}
  }

  implicit object TestMonoid extends Monoid[Double] with HasNegation[Double] {
    override def zero: Double = 0
    override def plus(lhs: Double, rhs: Double): Double = lhs + rhs
    override def negate(arg: Double): Double = -arg
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
