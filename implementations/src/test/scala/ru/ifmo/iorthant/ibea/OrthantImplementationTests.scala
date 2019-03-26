package ru.ifmo.iorthant.ibea

import scala.reflect.ClassTag

class OrthantImplementationTests extends Tests {
  override def makeAlgorithm[T : ClassTag](kappa: Double, maxSize: Int, dimension: Int): EpsilonIBEAFitness[T] = {
    new OrthantImplementation[T](kappa, maxSize, dimension)
  }
}
