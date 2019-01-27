package ru.ifmo.iorthant.ibea

abstract class EpsilonIBEAFitness[T] {
  def addIndividual(genotype: T, fitness: Array[Double]): Unit
  def trimPopulation(size: Int): Unit
  def fillPopulation(target: Array[T]): Unit
}
