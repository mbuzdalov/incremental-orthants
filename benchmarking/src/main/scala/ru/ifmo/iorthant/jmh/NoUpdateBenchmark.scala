package ru.ifmo.iorthant.jmh

import java.util.Random
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.*
import ru.ifmo.iorthant.noq2d.{NoUpdateIncrementalOrthantSearch, PlainArray, SimpleKD}
import ru.ifmo.iorthant.util.{DataGenerator, HasNegation, LiveDeadSet, Monoid}

import scala.compiletime.uninitialized

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.SECONDS)
@Timeout(time = 1, timeUnit = TimeUnit.HOURS)
@Warmup(iterations = 1, time = 6)
@Measurement(iterations = 1, time = 1)
@Fork(value = 5)
class NoUpdateBenchmark:
  import NoUpdateBenchmark.*

  //noinspection VarCouldBeVal: this inspection shall be suppressed for everything @Param
  @Param(Array("10", "31", "100", "316", "1000", "3162"))
  private var n: Int = uninitialized

  //noinspection VarCouldBeVal: this inspection shall be suppressed for everything @Param
  @Param(Array("2", "3", "4", "5", "7", "10"))
  private var d: Int = uninitialized

  //noinspection VarCouldBeVal: this inspection shall be suppressed for everything @Param
  @Param(Array("plain", "kd-simple"))
  private var algorithm: String = uninitialized

  //noinspection VarCouldBeVal: this inspection shall be suppressed for everything @Param
  @Param(Array("plane", "cube", "line"))
  private var test: String = uninitialized

  private var instances: Array[Array[Action]] = uninitialized

  @Setup
  def initialize(): Unit = instances = Array.tabulate(3): i =>
    // intentionally do not depend on "algorithm"
    val rng = Random(i * 72433566236111L + n * 623432 + d * 91274635553235L + test.hashCode)
    val queryIndices, dataIndices = LiveDeadSet(n)
    val actions = Array.newBuilder[Action]
    var wasQueryFull, wasDataFull = false
    val generator = DataGenerator.lookup(test)
    for _ <- 0 until 4 * n do
      if rng.nextBoolean() then
        if rng.nextDouble() < (1 - math.pow(2, queryIndices.nLive - n)) / (1 - math.pow(2, -n))
        then actions += AddQuery(generator.generate(rng, d), queryIndices.reviveRandom(rng)) // add a query
        else actions += RemoveQuery(queryIndices.killRandom(rng)) // delete a query
      else
        if rng.nextDouble() < (1 - math.pow(2, dataIndices.nLive - n)) / (1 - math.pow(2, -n))
        then actions += AddData(generator.generate(rng, d), rng.nextDouble(), dataIndices.reviveRandom(rng)) // add a query
        else actions += RemoveData(dataIndices.killRandom(rng)) // delete a query
        
      wasDataFull |= dataIndices.nDead == 0
      wasQueryFull |= queryIndices.nDead == 0
    assert(n < 100 || wasDataFull && wasQueryFull, s"$n: $wasDataFull, $wasQueryFull")
    actions.result()
  end initialize
  
  @OperationsPerInvocation(3)
  @Benchmark
  def benchmark(): Unit =
    for actions <- instances do
      val w = AlgorithmWrapper(algorithm, n, n)
      for action <- actions do
        action.perform(w)
end NoUpdateBenchmark

private object NoUpdateBenchmark:
  private class AlgorithmWrapper(algorithmName: String, nDataPoints: Int, nQueryPoints: Int):
    final val algorithm: NoUpdateIncrementalOrthantSearch[Double] = algorithmName match
      case "plain"             => PlainArray[Double]()
      case "kd-simple"         => SimpleKD[Double](0)
    final val dataPoints = algorithm.newDataPointHandleArray(nDataPoints)
    final val queryPoints = algorithm.newQueryPointHandleArray(nQueryPoints)

  private object IgnoreTracker extends NoUpdateIncrementalOrthantSearch.UpdateTracker[Double, AnyRef]:
    override def valueChanged(delta: Double, identifier: AnyRef): Unit = {}

  given Monoid[Double] with HasNegation[Double]:
    override def zero: Double = 0
    override def plus(lhs: Double, rhs: Double): Double = lhs + rhs
    override def negate(arg: Double): Double = -arg

  private abstract class Action:
    def perform(w: AlgorithmWrapper): Unit

  private class AddQuery(point: Array[Double], index: Int) extends Action:
    override def perform(w: AlgorithmWrapper): Unit =
      w.queryPoints(index) = w.algorithm.addQueryPoint(point, IgnoreTracker, IgnoreTracker)

  private class AddData(point: Array[Double], value: Double, index: Int) extends Action:
    override def perform(w: AlgorithmWrapper): Unit =
      w.dataPoints(index) = w.algorithm.addDataPoint(point, value)

  private class RemoveQuery(index: Int) extends Action:
    override def perform(w: AlgorithmWrapper): Unit =
      w.algorithm.removeQueryPoint(w.queryPoints(index))

  private class RemoveData(index: Int) extends Action:
    override def perform(w: AlgorithmWrapper): Unit =
      w.algorithm.removeDataPoint(w.dataPoints(index))
end NoUpdateBenchmark

