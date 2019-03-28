package ru.ifmo.iorthant.ibea

import java.util.{Arrays => JArrays}

import ru.ifmo.iorthant.noq2d.{NoUpdateIncrementalOrthantSearch, SimpleKD}
import ru.ifmo.iorthant.util.{HasNegation, Monoid, PriorityQueueWithReferences}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class OrthantImplementation[T](kappa: Double, maxIndividuals: Int, dimension: Int) extends EpsilonIBEAFitness[T] {
  import OrthantImplementation._

  private[this] implicit final val m: DefaultDoubleMonoid = defaultDoubleMonoid
  private[this] final val trees = Array.tabulate(dimension)(i => new SimpleKD[Double](i))
  private[this] final val queue = new PriorityQueueWithReferences[IndividualHolder[T]](maxIndividuals)
  private[this] final val hash0 = new HasherType[T]()

  override def size: Int = queue.size

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
      removalHolders(d) = new RemovalHolder(trees(d), projectPoint(fitness, d), d, 1.0 / multipliers(d), holder, hash0)
      d += 1
    }
  }
  override def trimPopulation(size: Int): Unit = {
    while (queue.size > size) {
      val worst = queue.removeSmallest()
      val holders = worst.holders
      var d = holders.length - 1
      while (d >= 0) {
        holders(d).removeMe(trees(d), d, hash0)
        d -= 1
      }
    }
  }
  override def fillPopulation(target: Array[T]): Unit = queue.foreachWithIndex((h, i) => target(i) = h.genotype)
  override def iterateOverPotentials(fun: (T, Double) => Unit): Unit = queue.foreach(h => fun(h.genotype, h.fitness))
}

object OrthantImplementation {
  private type HasherType[T] = mutable.HashMap[RemovalHolder[T], ArrayBuffer[RemovalHolder[T]]]

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

  private class RemovalHolder[T](tree: SimpleKD[Double], point: Array[Double],
                                 index: Int, value: Double, holder: IndividualHolder[T],
                                 hash0: HasherType[T])
                                (implicit m: Monoid[Double]) {
    private val queryPoint = tree.addQueryPoint(point, holder, index)
    private val dataPoint = tree.addDataPoint(point, value)

    if (index == 0) {
      val buf = hash0.getOrElseUpdate(this, new ArrayBuffer(2))
      for (other <- buf) {
        queryPoint.plus(other.dataPoint.value)
        other.queryPoint.plus(value)
      }
      buf += this
    }

    def removeMe(tree: SimpleKD[Double],
                 index: Int,
                 hash0: HasherType[T])(implicit m: HasNegation[Double]): Unit = {
      tree.removeDataPoint(dataPoint)
      tree.removeQueryPoint(queryPoint)
      if (index == 0) {
        val buf = hash0(this)
        removeExactlyMe(buf, buf.size - 1)
        val value = dataPoint.value
        for (other <- buf) {
          queryPoint.minus(other.dataPoint.value)
          other.queryPoint.minus(value)
        }
      }
    }

    private def removeExactlyMe(buf: ArrayBuffer[RemovalHolder[T]], i: Int): Unit = {
      if (buf(i) eq this) {
        buf.remove(i)
      } else {
        removeExactlyMe(buf, i - 1)
      }
    }

    // this class also acts as a hash tag for the index == 0 case.

    override def hashCode(): Int = JArrays.hashCode(dataPoint.point)

    override def equals(obj: Any): Boolean = obj match {
      case h: RemovalHolder[T] => JArrays.equals(dataPoint.point, h.dataPoint.point)
      case _ => false
    }
  }

  private class DefaultDoubleMonoid extends Monoid[Double] with HasNegation[Double] {
    override def zero: Double = 0
    override def plus(lhs: Double, rhs: Double): Double = lhs + rhs
    override def negate(arg: Double): Double = -arg
  }
  private val defaultDoubleMonoid = new DefaultDoubleMonoid

  private class IndividualHolder[T](val genotype: T,
                                    multipliers: Array[Double],
                                    val holders: Array[RemovalHolder[T]],
                                    queue: PriorityQueueWithReferences[IndividualHolder[T]])
    extends PriorityQueueWithReferences.HasIndex
      with Ordered[IndividualHolder[T]]
      with NoUpdateIncrementalOrthantSearch.UpdateTracker[Double, Int]
  {
    var fitness: Double = 0.0
    override def compare(that: IndividualHolder[T]): Int = java.lang.Double.compare(fitness, that.fitness)

    override def valueChanged(delta: Double, identifier: Int): Unit = {
      fitness -= delta * multipliers(identifier)
      if (delta < 0) {
        queue.updateAfterIncrease(this)
      } else {
        queue.updateAfterDecrease(this)
      }
    }
  }
}
