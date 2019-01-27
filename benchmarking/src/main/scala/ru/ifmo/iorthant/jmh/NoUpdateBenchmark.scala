package ru.ifmo.iorthant.jmh

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import ru.ifmo.iorthant.noq2d.{NoUpdateIncrementalOrthantSearch, PlainArray, SimpleKD}
import ru.ifmo.iorthant.util.{HasMinus, Monoid}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Timeout(time = 1, timeUnit = TimeUnit.HOURS)
@Warmup(iterations = 1, time = 6)
@Measurement(iterations = 1, time = 1)
@Fork(value = 5)
class NoUpdateBenchmark {
  import NoUpdateBenchmark._

  @Param(Array("10", "31", "100", "316", "1000", "3162", "10000"))
  private var n: Int = _

  @Param(Array("2", "3", "4", "5"))
  private var d: Int = _

  @Param(Array("plain", "kd-simple"))
  private var algo: String = _

  @Param(Array("plane", "cube", "line"))
  private var test: String = _

  private var instances: Array[Array[Action]] = _
  private var nDataPoints, nQueryPoints: Int = _

  @Setup
  def initialize(): Unit = {

  }

  @Benchmark
  def benchmark(bh: Blackhole): Unit = {
    for (actions <- instances) {
      val w = new AlgorithmWrapper(algo, nDataPoints, nQueryPoints)
      for (action <- actions) {
        action.perform(w)
      }
    }
  }
}

object NoUpdateBenchmark {
  class AlgorithmWrapper(algorithmName: String, nDataPoints: Int, nQueryPoints: Int) {
    final val algorithm: NoUpdateIncrementalOrthantSearch[Double] = algorithmName match {
      case "plain"     => new PlainArray[Double]()(TestMonoid)
      case "kd-simple" => new SimpleKD[Double]()(TestMonoid)
    }
    import algorithm._ // this import is necessary for the following two statements to work
    final val dataPoints = new Array[algorithm.DataPointHandle](nDataPoints)
    final val queryPoints = new Array[algorithm.QueryPointHandle](nDataPoints)
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
