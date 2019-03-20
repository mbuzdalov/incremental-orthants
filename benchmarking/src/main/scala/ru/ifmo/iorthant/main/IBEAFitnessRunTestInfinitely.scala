package ru.ifmo.iorthant.main

import java.util.Random

import ru.ifmo.iorthant.ibea.{NaiveImplementation, OrthantImplementation}
import ru.ifmo.iorthant.util.DataGenerator

object IBEAFitnessRunTestInfinitely {
  def main(args: Array[String]): Unit = {
    val defaultKappa = 0.05

    val liftedArgs = args.lift
    val n = liftedArgs(0) match {
      case Some(s) => s.toInt
      case None => sys.error("Please specify the number of points N as the 1st argument")
    }
    val maxPoints = n * 2

    val d = liftedArgs(1) match {
      case Some(s) => s.toInt
      case None => sys.error("Please specify the dimension D as the 2nd argument")
    }

    val algorithm = liftedArgs(2) match {
      case None => sys.error("Please specify the algorithm to run as the 3rd argument")
      case Some("naive") => new NaiveImplementation[Unit](defaultKappa, maxPoints)
      case Some("orthant") => new OrthantImplementation[Unit](defaultKappa, maxPoints, d)
      case Some(s) => sys.error(s"I don't know the algorithm '$s' given as the 3rd argument")
    }

    val generator = liftedArgs(3) match {
      case Some(s) => DataGenerator.lookup(s)
      case None => sys.error("Please specify the test generator (either of: cube plain line) as the 4th argument")
    }

    val instance = Array.fill(10 * n)(generator.generate(new Random(), d))
    while (true) {
      val t = System.nanoTime()
      for (p <- instance) {
        algorithm.addIndividual((), p)
        if (algorithm.size >= maxPoints) {
          algorithm.trimPopulation(n)
        }
      }
      algorithm.trimPopulation(0)
      println(s"Time: ${(System.nanoTime() - t) * 1e-9} s")
    }
  }
}
