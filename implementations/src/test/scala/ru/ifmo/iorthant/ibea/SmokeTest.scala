package ru.ifmo.iorthant.ibea

import java.util.Random

import org.junit.{Assert, Test}

import scala.collection.mutable

class SmokeTest {
  private def assertEquals(a: Double, b: Double, iteration: Int): Unit = {
    val scale = math.max(1, math.max(math.abs(a), math.abs(b)))
    if (math.abs(a - b) > scale * 1e-12) {
      throw new AssertionError("Iteration " + iteration + ": Expected " + a + " found " + b)
    }
  }

  private def compare(a: EpsilonIBEAFitness[String], b: EpsilonIBEAFitness[String], iteration: Int): Unit = {
    val hash = new mutable.HashMap[String, Double]()
    a.iterateOverPotentials((g, f) => hash += g -> f)
    b.iterateOverPotentials((g, f) => assertEquals(hash(g), f, iteration))
  }

  @Test
  def smoke(): Unit = {
    val dim = 2
    val naive = new NaiveImplementation[String](0.05, 100)
    val orthant = new OrthantImplementation[String](0.05, 100, dim)
    val rng = new Random(667345823536361L)
    for (z <- 0 to 10000) {
      Assert.assertEquals(naive.size, orthant.size)
      compare(naive, orthant, z)
      if (naive.size == 100 || naive.size > 90 && rng.nextBoolean()) {
        val newSize = 1 + rng.nextInt(75)
        naive.trimPopulation(newSize)
        orthant.trimPopulation(newSize)
      } else {
        val newItem = Array.fill(dim)(rng.nextDouble())
        val newGenotype = z.toString
        naive.addIndividual(newGenotype, newItem)
        orthant.addIndividual(newGenotype, newItem)
      }
    }
  }
}
