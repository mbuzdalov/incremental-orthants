package ru.ifmo.iorthant.ibea

import scala.reflect.ClassTag

class NaiveImplementation[T : ClassTag](kappa: Double, maxIndividuals: Int) extends EpsilonIBEAFitness[T]:
  private val individuals = Array.ofDim[T](maxIndividuals)
  private val objectives = Array.ofDim[Array[Double]](maxIndividuals)
  private val potentials = Array.ofDim[Double](maxIndividuals)
  private var count = 0
  private val nullIndividual = individuals(0) // a cheap way to find a "null: T"

  override def size: Int = count

  override def addIndividual(genotype: T, fitness: Array[Double]): Unit =
    individuals(count) = genotype
    objectives(count) = fitness
    potentials(count) = collectIndicatorSum(fitness, count)
    subtractFromPotentials(fitness, count)
    count += 1

  override def trimPopulation(size: Int): Unit =
    while count > size do
      var worst = 0
      var worstV = potentials(0)
      var i = 1
      while i < count do
        val currV = potentials(i)
        if worstV > currV then
          worstV = currV
          worst = i
        i += 1
      count -= 1

      val ow = objectives(worst)
      individuals(worst) = individuals(count)
      individuals(count) = nullIndividual
      objectives(worst) = objectives(count)
      objectives(count) = null
      potentials(worst) = potentials(count)
      addToPotentials(ow, count)

  override def fillPopulation(target: Array[T]): Unit =
    System.arraycopy(individuals, 0, target, 0, count)

  override def iterateOverPotentials(fun: (T, Double) => Unit): Unit =
    var i = 0
    while i < count do
      fun(individuals(i), potentials(i))
      i += 1

  private def indicator(lhs: Array[Double], rhs: Array[Double], length: Int): Double = 
    var result = rhs(0) - lhs(0)
    var index = 1
    while index < length do
      result = math.min(result, rhs(index) - lhs(index))
      index += 1
    math.exp(result / kappa)

  private def collectIndicatorSum(fitness: Array[Double], index: Int): Double =
    val len = fitness.length
    var i = index - 1
    var sum = 0.0
    while i >= 0 do
      sum -= indicator(objectives(i), fitness, len)
      i -= 1
    sum

  private def subtractFromPotentials(fitness: Array[Double], index: Int): Unit =
    val len = fitness.length
    var i = index - 1
    while i >= 0 do
      potentials(i) -= indicator(fitness, objectives(i), len)
      i -= 1

  private def addToPotentials(fitness: Array[Double], index: Int): Unit =
    val len = fitness.length
    var i = index - 1
    while i >= 0 do
      potentials(i) += indicator(fitness, objectives(i), len)
      i -= 1
