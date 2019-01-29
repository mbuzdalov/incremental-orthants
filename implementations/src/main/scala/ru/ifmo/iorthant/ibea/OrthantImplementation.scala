package ru.ifmo.iorthant.ibea

import ru.ifmo.iorthant.noq2d.{NoUpdateIncrementalOrthantSearch, SimpleKD}
import ru.ifmo.iorthant.util.{HasMinus, Monoid, PriorityQueueWithReferences}

import scala.reflect.ClassTag

class OrthantImplementation[T : ClassTag](kappa: Double, maxIndividuals: Int, dimension: Int) extends EpsilonIBEAFitness[T] {
  import OrthantImplementation._

  private[this] implicit final val m: DefaultDoubleMonoid = defaultDoubleMonoid
  private[this] final val trees = Array.tabulate(dimension)(i => new SimpleKD[Double](i))
  private[this] final val queue = new PriorityQueueWithReferences[IndividualHolder[T]](maxIndividuals)

  override def addIndividual(genotype: T, fitness: Array[Double]): Unit = {
    val removalHolders = new Array[RemovalHolder](dimension)
    val holder = new IndividualHolder[T](genotype, removalHolders, queue)
    var d = 0
    while (d < dimension) {
      removalHolders(d) = new RemovalHolder(trees(d), projectPoint(fitness, d), ???, holder)
      d += 1
    }
    queue.add(holder)
  }
  override def trimPopulation(size: Int): Unit = {
    while (queue.size > size) {
      val worst = queue.removeSmallest()
      worst.holders.foreach(_.removeMe)
    }
  }
  override def fillPopulation(target: Array[T]): Unit = queue.foreachWithIndex((h, i) => target(i) = h.genotype)
}

object OrthantImplementation {
  private def projectPoint(src: Array[Double], index: Int): Array[Double] = {
    val len = src.length
    val ref = src(index)
    val rv = new Array[Double](len - 1)
    var i = len - 1
    while (i > index) {
      val v = ref - src(i)
      i -= 1
      rv(i) = v
    }
    while (i > 0) {
      i -= 1
      rv(i) = ref - src(i)
    }
    rv
  }

  private class RemovalHolder(tree: SimpleKD[Double], point: Array[Double], value: Double, holder: IndividualHolder[_]) {
    private val queryPoint = tree.addQueryPoint(point, holder, ())
    private val dataPoint = tree.addDataPoint(point, value)

    def removeMe(implicit m: HasMinus[Double]): Unit = {
      tree.removeDataPoint(dataPoint)
      tree.removeQueryPoint(queryPoint)
    }
  }

  private class DefaultDoubleMonoid extends Monoid[Double] with HasMinus[Double] {
    override def zero: Double = 0
    override def plus(lhs: Double, rhs: Double): Double = lhs + rhs
    override def minus(lhs: Double, rhs: Double): Double = lhs - rhs
  }
  private val defaultDoubleMonoid = new DefaultDoubleMonoid

  private class IndividualHolder[T](val genotype: T,
                                    val holders: Array[RemovalHolder],
                                    queue: PriorityQueueWithReferences[IndividualHolder[T]])
    extends PriorityQueueWithReferences.HasIndex
      with Ordered[IndividualHolder[T]]
      with NoUpdateIncrementalOrthantSearch.UpdateTracker[Double, Unit]
  {
    var fitness: Double = 0.0
    override def compare(that: IndividualHolder[T]): Int = java.lang.Double.compare(fitness, that.fitness)

    override def valueChanged(oldValue: Double, newValue: Double, identifier: Unit): Unit = {
      // TODO: NOT LIKE THAT!!!1
      fitness += newValue - oldValue
      if (newValue > oldValue) {
        queue.updateAfterIncrease(this)
      } else {
        queue.updateAfterDecrease(this)
      }
    }
  }
}
