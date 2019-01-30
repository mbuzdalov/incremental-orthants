package ru.ifmo.iorthant.ibea

abstract class EpsilonIBEAFitness[T] {
  def addIndividual(genotype: T, fitness: Array[Double]): Unit
  def size: Int
  def trimPopulation(size: Int): Unit
  def fillPopulation(target: Array[T]): Unit
  def iterateOverPotentials(fun: (T, Double) => Unit): Unit
}
