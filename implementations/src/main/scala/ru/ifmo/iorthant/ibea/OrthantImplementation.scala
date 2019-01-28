package ru.ifmo.iorthant.ibea

import ru.ifmo.iorthant.noq2d.SimpleKD
import ru.ifmo.iorthant.util.{HasMinus, Monoid, PriorityQueueWithReferences}

import scala.reflect.ClassTag

class OrthantImplementation[T : ClassTag](kappa: Double, maxIndividuals: Int, dimension: Int) extends EpsilonIBEAFitness[T] {
  import OrthantImplementation._

  private[this] implicit final val m: DefaultDoubleMonoid = defaultDoubleMonoid
  private[this] final val trees = Array.tabulate(dimension)(i => new SimpleKD[Double](i))
  private[this] final val queue = new PriorityQueueWithReferences[IndividualHolder[T]](maxIndividuals)

  override def addIndividual(genotype: T, fitness: Array[Double]): Unit = ???
  override def trimPopulation(size: Int): Unit = ???
  override def fillPopulation(target: Array[T]): Unit = ???
}

object OrthantImplementation {
  private class DefaultDoubleMonoid extends Monoid[Double] with HasMinus[Double] {
    override def zero: Double = 0
    override def plus(lhs: Double, rhs: Double): Double = lhs + rhs
    override def minus(lhs: Double, rhs: Double): Double = lhs - rhs
  }
  private val defaultDoubleMonoid = new DefaultDoubleMonoid

  private class IndividualHolder[T](val genotype: T)
    extends PriorityQueueWithReferences.HasIndex with Ordered[IndividualHolder[T]] {
    var fitness: Double = 0.0
    override def compare(that: IndividualHolder[T]): Int = java.lang.Double.compare(fitness, that.fitness)
  }
}
