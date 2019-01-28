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

  @Test
  def testEqualPoints(): Unit = {
    val a = makeAlgorithm[String](defaultKappa, 9)
    val place = new Array[String](6)
    a.addIndividual("i0", Array(1, 0, 0))
    a.addIndividual("i0", Array(1, 0, 0))
    a.addIndividual("i1", Array(0, 1, 0))
    a.addIndividual("i1", Array(0, 1, 0))
    a.addIndividual("i1", Array(0, 1, 0))
    a.addIndividual("i2", Array(0, 0, 1))
    a.addIndividual("i2", Array(0, 0, 1))
    a.addIndividual("i2", Array(0, 0, 1))
    a.addIndividual("i2", Array(0, 0, 1))
    a.trimPopulation(6)
    a.fillPopulation(place)

    scala.util.Sorting.quickSort(place)
    Assert.assertEquals("i0", place(0))
    Assert.assertEquals("i0", place(1))
    Assert.assertEquals("i1", place(2))
    Assert.assertEquals("i1", place(3))
    Assert.assertEquals("i2", place(4))
    Assert.assertEquals("i2", place(5))
  }
}

object Tests {
  class NaiveImplementationTests extends Tests {
    override def makeAlgorithm[T : ClassTag](kappa: Double, maxSize: Int): EpsilonIBEAFitness[T] = {
      new NaiveImplementation[T](kappa, maxSize)
    }
  }
}
