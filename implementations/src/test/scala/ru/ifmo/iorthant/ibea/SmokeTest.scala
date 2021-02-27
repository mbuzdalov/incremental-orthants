package ru.ifmo.iorthant.ibea

import java.util.Random

import scala.annotation.tailrec
import scala.collection.mutable

import org.junit.{Assert, Test}

class SmokeTest {
  private def differ(a: Double, b: Double): Boolean = {
    val scale = math.max(1, math.max(math.abs(a), math.abs(b)))
    math.abs(a - b) > scale * 1e-12
  }

  private def assertEquals(a: Double, b: Double, iteration: Int): Unit = {
    if (differ(a, b)) {
      throw new AssertionError("Iteration " + iteration + ": Expected " + a + " found " + b)
    }
  }

  @tailrec
  private def validate(iteration: Int, tuples: IndexedSeq[(Double, Int)], index: Int, last: Double, lastCount: Int): Unit = {
    if (index == tuples.size || differ(last, tuples(index)._1) && lastCount != 0) {
      if (lastCount != 0) {
        throw new AssertionError("Iteration " + iteration + s": Unbalanced non-common keys found: for value $last the balance is $lastCount")
      }
    } else {
      val (value, count) = tuples(index)
      if (differ(last, value)) {
        assert(lastCount == 0)
        validate(iteration, tuples, index + 1, value, count)
      } else {
        validate(iteration, tuples, index + 1, value, lastCount + count)
      }
    }
  }

  private def compare(a: EpsilonIBEAFitness[String], b: EpsilonIBEAFitness[String], iteration: Int): Unit = {
    val keysOfA, keysOfB = new mutable.HashSet[String]
    a.iterateOverPotentials((g, _) => keysOfA += g)
    b.iterateOverPotentials((g, _) => keysOfB += g)
    val commonKeys = keysOfA.intersect(keysOfB).filter(_ => false)
    val commonHash = new mutable.HashMap[String, Double]()
    val nonCommonMultiSet = new mutable.HashMap[Double, Int]()
    a.iterateOverPotentials((g, f) => if (commonKeys.contains(g)) commonHash.update(g, f) else nonCommonMultiSet.update(f, nonCommonMultiSet.getOrElse(f, 0) + 1))
    b.iterateOverPotentials((g, f) => if (commonKeys.contains(g)) assertEquals(commonHash(g), f, iteration) else nonCommonMultiSet.update(f, nonCommonMultiSet.getOrElse(f, 0) - 1))
    val nonCommonNonZero = nonCommonMultiSet.filter(_._2 != 0)
    if (nonCommonNonZero.nonEmpty) {
      val sorted = nonCommonNonZero.toIndexedSeq.sortBy(_._1)
      validate(iteration, sorted, 0, Double.NaN, 0)
    }
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

  @Test
  def smokeInt(): Unit = {
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
        val newItem = Array.fill(dim)(rng.nextInt(5).toDouble).sorted
        val newGenotype = z.toString
        naive.addIndividual(newGenotype, newItem)
        orthant.addIndividual(newGenotype, newItem)
      }
    }
  }
}
