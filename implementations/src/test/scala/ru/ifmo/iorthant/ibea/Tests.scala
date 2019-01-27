package ru.ifmo.iorthant.ibea

import scala.reflect.ClassTag

import org.junit.{Assert, Test}

abstract class Tests {
  private final val defaultKappa = 0.05

  def makeAlgorithm[T : ClassTag](kappa: Double, maxSize: Int): EpsilonIBEAFitness[T]

  @Test
  def testTwoDominatingPoints(): Unit = {
    val a = makeAlgorithm[String](defaultKappa, 2)
    val place = new Array[String](1)
    a.addIndividual("i0", Array(1, 1))
    a.addIndividual("i1", Array(2, 2))
    a.trimPopulation(1)
    a.fillPopulation(place)
    Assert.assertEquals("i0", place(0))
  }

  @Test
  def testTwoNonDominatingPoints(): Unit = {
    val a = makeAlgorithm[String](defaultKappa, 2)
    val place = new Array[String](1)
    a.addIndividual("i0", Array(0, 2))
    a.addIndividual("i1", Array(1, 0))
    a.trimPopulation(1)
    a.fillPopulation(place)
    Assert.assertEquals("i1", place(0))
  }

  @Test
  def testThreeNonDominatingPoints(): Unit = {
    val a = makeAlgorithm[String](defaultKappa, 3)
    val place = new Array[String](2)
    a.addIndividual("i0", Array(0, 2))
    a.addIndividual("i1", Array(1, 1))
    a.addIndividual("i2", Array(2, 0))
    a.trimPopulation(2)
    a.fillPopulation(place)

    scala.util.Sorting.quickSort(place)
    Assert.assertEquals("i0", place(0))
    Assert.assertEquals("i2", place(1))
  }
}

object Tests {
  class NaiveImplementationTests extends Tests {
    override def makeAlgorithm[T : ClassTag](kappa: Double, maxSize: Int): EpsilonIBEAFitness[T] = {
      new NaiveImplementation[T](kappa, maxSize)
    }
  }
}
