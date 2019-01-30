package ru.ifmo.iorthant.ibea

import ru.ifmo.iorthant.noq2d.{NoUpdateIncrementalOrthantSearch, SimpleKD}
import ru.ifmo.iorthant.util.{HasMinus, Monoid, PriorityQueueWithReferences}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class OrthantImplementation[T](kappa: Double, maxIndividuals: Int, dimension: Int) extends EpsilonIBEAFitness[T] {
  import OrthantImplementation._

  private[this] implicit final val m: DefaultDoubleMonoid = defaultDoubleMonoid
  private[this] final val trees = Array.tabulate(dimension)(i => new SimpleKD[Double](i))
  private[this] final val queue = new PriorityQueueWithReferences[IndividualHolder[T]](maxIndividuals)
  private[this] final val hash0 = new mutable.HashMap[RemovalHolder[T], ArrayBuffer[IndividualHolder[T]]]()

  override def addIndividual(genotype: T, fitness: Array[Double]): Unit = {
    val removalHolders = new Array[RemovalHolder[T]](dimension)
    val multipliers = new Array[Double](dimension)
    var d = 0
    while (d < dimension) {
      multipliers(d) = math.exp(fitness(d) / kappa)
      d += 1
    }
    val holder = new IndividualHolder[T](genotype, multipliers, removalHolders, queue)
    queue.add(holder)
    d = 0
    while (d < dimension) {
      removalHolders(d) = new RemovalHolder(trees(d), projectPoint(fitness, d), d, math.exp(-fitness(d) / kappa), holder, hash0)
      d += 1
    }
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
      val v = src(i) - ref
      i -= 1
      rv(i) = v
    }
    while (i > 0) {
      i -= 1
      rv(i) = src(i) - ref
    }
    rv
  }

  private class RemovalHolder[T](tree: SimpleKD[Double], private val point: Array[Double],
                                 index: Int, private val value: Double, holder: IndividualHolder[T],
                                 hash0: mutable.HashMap[RemovalHolder[T], ArrayBuffer[IndividualHolder[T]]])
                                (implicit m: Monoid[Double]) {
    private val queryPoint = tree.addQueryPoint(point, holder, index)
    private val dataPoint = tree.addDataPoint(point, value)

    if (index == 0) {
      val buf = hash0.getOrElseUpdate(this, new ArrayBuffer())
      for (other <- buf) {
        val otherH = other.holders(0)
        queryPoint.plus(otherH.value)
        otherH.queryPoint.plus(value)
      }
      buf += holder
    }

    def removeMe(implicit m: HasMinus[Double]): Unit = {
      tree.removeDataPoint(dataPoint)
      tree.removeQueryPoint(queryPoint)
      if (index == 0) {
        val buf = hash0(this)
        buf -= holder
        for (other <- buf) {
          val otherH = other.holders(0)
          queryPoint.minus(otherH.value)
          otherH.queryPoint.minus(value)
        }
      }
    }

    // this class also acts as a hash tag for the index == 0 case.

    override def hashCode(): Int = java.util.Arrays.hashCode(point)

    override def equals(obj: Any): Boolean = obj match {
      case h: RemovalHolder[T] => java.util.Arrays.equals(point, h.point)
      case _ => false
    }
  }

  private class DefaultDoubleMonoid extends Monoid[Double] with HasMinus[Double] {
    override def zero: Double = 0
    override def plus(lhs: Double, rhs: Double): Double = lhs + rhs
    override def minus(lhs: Double, rhs: Double): Double = lhs - rhs
  }
  private val defaultDoubleMonoid = new DefaultDoubleMonoid

  private class IndividualHolder[T](val genotype: T,
                                    val multipliers: Array[Double],
                                    val holders: Array[RemovalHolder[T]],
                                    queue: PriorityQueueWithReferences[IndividualHolder[T]])
    extends PriorityQueueWithReferences.HasIndex
      with Ordered[IndividualHolder[T]]
      with NoUpdateIncrementalOrthantSearch.UpdateTracker[Double, Int]
  {
    var fitness: Double = 0.0
    override def compare(that: IndividualHolder[T]): Int = java.lang.Double.compare(fitness, that.fitness)

    override def valueChanged(oldValue: Double, newValue: Double, identifier: Int): Unit = {
      val prevFitness = fitness
      fitness -= (newValue - oldValue) * multipliers(identifier)
      if (prevFitness < fitness) {
        queue.updateAfterIncrease(this)
      } else {
        queue.updateAfterDecrease(this)
      }
    }
  }
}
