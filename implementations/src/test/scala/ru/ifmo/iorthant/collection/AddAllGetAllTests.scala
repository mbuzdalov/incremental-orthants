package ru.ifmo.iorthant.collection

import java.util.Random

import scala.collection.mutable.ArrayBuffer

import org.junit.{Assert, Test}

import ru.ifmo.iorthant.util.Dominance

class AddAllGetAllTests {
  private implicit val intMonoid: Monoid[Int] = TestMonoid

  private def smokeTest(dim: Int, ds: AddOne[Int] with AddPrefix[Int] with GetOne[Int] with GetPrefix[Int]): Unit = {
    class Wrapper(val point: Array[Double], val ref: ds.Handle, var value: Int)

    val rng = new Random(8239423952352124L)
    val pts = new ArrayBuffer[Wrapper]()

    def randomPoint(): Array[Double] = Array.fill(dim)(rng.nextDouble() * 1241521 - 882343)
    def randomValue(): Int = rng.nextInt(8235) - 4441
    def randomPointThatProbablyExists(): Array[Double] = {
      if (pts.size > 0 && rng.nextBoolean()) pts(rng.nextInt(pts.size)).point else randomPoint()
    }

    def domFilter(point: Array[Double]) = pts.withFilter(w => Dominance.strict(w.point, point))

    def addToPrefix(point: Array[Double], value: Int): Unit = domFilter(point).foreach(_.value += value)
    def getOnPrefix(point: Array[Double]): Int = domFilter(point).map(_.value).sum

    for (_ <- 0 until 1000) {
      rng.nextInt(4) match {
        case 0 => // add a random point
          val point = randomPoint()
          val value = randomValue()
          pts += new Wrapper(point, ds.add(point, value), value)
        case 1 => // test a known point
          if (pts.nonEmpty) {
            val index = rng.nextInt(pts.size)
            Assert.assertEquals(pts(index).value, ds.get(pts(index).ref))
          }
        case 2 => // add to a prefix
          val point = randomPointThatProbablyExists()
          val value = randomValue()
          addToPrefix(point, value)
          ds.addToPrefix(point, value)
        case 3 => // get prefix sum
          val point = randomPointThatProbablyExists()
          Assert.assertEquals(getOnPrefix(point), ds.getOnPrefix(point))
      }
    }
  }

  @Test def plainArray(): Unit = (1 to 5).foreach(d => smokeTest(d, new PlainArray[Int]()))
}
