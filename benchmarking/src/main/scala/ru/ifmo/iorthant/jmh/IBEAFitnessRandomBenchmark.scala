package ru.ifmo.iorthant.jmh

import java.util.Random
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

import ru.ifmo.iorthant.ibea.{NaiveImplementation, OrthantImplementation}
import ru.ifmo.iorthant.util.DataGenerator

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.SECONDS)
@Timeout(time = 1, timeUnit = TimeUnit.HOURS)
@Warmup(iterations = 1, time = 6)
@Measurement(iterations = 1, time = 1)
@Fork(value = 5)
class IBEAFitnessRandomBenchmark {

  //noinspection VarCouldBeVal: this inspection shall be suppressed for everything @Param
  @Param(Array("10", "31", "100", "316", "1000", "3162"))
  private var n: Int = _

  //noinspection VarCouldBeVal: this inspection shall be suppressed for everything @Param
  @Param(Array("2", "3", "4", "5", "7", "10"))
  private var d: Int = _

  //noinspection VarCouldBeVal: this inspection shall be suppressed for everything @Param
  @Param(Array("naive", "orthant"))
  private var algorithm: String = _

  //noinspection VarCouldBeVal: this inspection shall be suppressed for everything @Param
  @Param(Array("plane", "cube", "line"))
  private var test: String = _

  private var instances: Array[Array[Array[Double]]] = _

  @Setup
  def initialize(): Unit = instances = Array.tabulate(3) { i =>
    // intentionally do not depend on "algorithm"
    val rng = new Random(i * 72433566236111L + n * 623432 + d * 91274635553235L + test.hashCode)
    val generator = DataGenerator.lookup(test)
    Array.fill(4 * n)(generator.generate(rng, d))
  }

  @OperationsPerInvocation(3)
  @Benchmark
  def benchmark(): Unit = {
    val maxSize = n * 2
    for (instance <- instances) {
      val a = algorithm match {
        case "naive" => new NaiveImplementation[Unit](0.05, maxSize)
        case "orthant" => new OrthantImplementation[Unit](0.05, maxSize, d)
      }
      for (point <- instance) {
        a.addIndividual((), point)
        if (a.size == maxSize) {
          a.trimPopulation(n)
        }
      }
    }
  }
}
