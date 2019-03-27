package ru.ifmo.iorthant.ibea

import scala.reflect.ClassTag

class NaiveImplementation[T : ClassTag](kappa: Double, maxIndividuals: Int) extends EpsilonIBEAFitness[T] {
  private[this] val individuals = new Array[T](maxIndividuals)
  private[this] val objectives = new Array[Array[Double]](maxIndividuals)
  private[this] val potentials = new Array[Double](maxIndividuals)
  private[this] var count = 0

  override def size: Int = count

  override def addIndividual(genotype: T, fitness: Array[Double]): Unit = {
    individuals(count) = genotype
    objectives(count) = fitness
    potentials(count) = collectIndicatorSum(fitness, count)
    subtractFromPotentials(fitness, count)
    count += 1
  }

  override def trimPopulation(size: Int): Unit = {
    while (count > size) {
      var worst = 0
      var worstV = potentials(0)
      var i = 1
      while (i < count) {
        val currV = potentials(i)
        if (worstV > currV) {
          worstV = currV
          worst = i
        }
        i += 1
      }
      count -= 1

      val ow = objectives(worst)
      individuals(worst) = individuals(count)
      individuals(count) = _
      objectives(worst) = objectives(count)
      objectives(count) = _
      potentials(worst) = potentials(count)
      addToPotentials(ow, count)
    }
  }

  override def fillPopulation(target: Array[T]): Unit = {
    System.arraycopy(individuals, 0, target, 0, count)
  }

  override def iterateOverPotentials(fun: (T, Double) => Unit): Unit = {
    var i = 0
    while (i < count) {
      fun(individuals(i), potentials(i))
      i += 1
    }
  }

  private def indicator(lhs: Array[Double], rhs: Array[Double], length: Int): Double = {
    var result = rhs(0) - lhs(0)
    var index = 1
    while (index < length) {
      result = math.min(result, rhs(index) - lhs(index))
      index += 1
    }
    math.exp(result / kappa)
  }

  private def collectIndicatorSum(fitness: Array[Double], index: Int): Double = {
    val len = fitness.length
    var i = index - 1
    var sum = 0.0
    while (i >= 0) {
      sum -= indicator(objectives(i), fitness, len)
      i -= 1
    }
    sum
  }

  private def subtractFromPotentials(fitness: Array[Double], index: Int): Unit = {
    val len = fitness.length
    var i = index - 1
    while (i >= 0) {
      potentials(i) -= indicator(fitness, objectives(i), len)
      i -= 1
    }
  }

  private def addToPotentials(fitness: Array[Double], index: Int): Unit = {
    val len = fitness.length
    var i = index - 1
    while (i >= 0) {
      potentials(i) += indicator(fitness, objectives(i), len)
      i -= 1
    }
  }
}
