package ru.ifmo.iorthant.util

import java.util.Random

import ru.ifmo.iorthant.util.Syntax._

trait DataGenerator {
  def generate(rng: Random, d: Int): Array[Double]
}

object DataGenerator {
  object Cube extends DataGenerator {
    override def generate(rng: Random, d: Int): Array[Double] = Array.fill(d)(rng.nextDouble())
  }
  object Line extends DataGenerator {
    override def generate(rng: Random, d: Int): Array[Double] = {
      val v = rng.nextDouble()
      Array.fill(d)(v)
    }
  }
  object Plane extends DataGenerator {
    override def generate(rng: Random, d: Int): Array[Double] = {
      Array.fill(d)(rng.nextDouble()).whereAlso(a => a(0) += 1.0 - a.sum)
    }
  }

  def lookup(name: String): DataGenerator = name match {
    case "cube" => Cube
    case "line" => Line
    case "plane" => Plane
    case _ => throw new IllegalArgumentException(s"Unknown generator requested: '$name'")
  }
}
