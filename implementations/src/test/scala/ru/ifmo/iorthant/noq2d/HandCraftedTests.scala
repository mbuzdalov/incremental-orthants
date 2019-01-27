package ru.ifmo.iorthant.noq2d

import java.util.Random

import scala.collection.mutable.ArrayBuffer

import org.junit.{Assert, Test}

import ru.ifmo.iorthant.util.Dominance

abstract class HandCraftedTests {
  protected implicit final val m: DefaultIntMonoid.type = DefaultIntMonoid

  def makeDataStructure(): NoUpdateIncrementalOrthantSearch[Int]

  @Test
  def oneQueryOneDataNonDominated(): Unit = {
    val ds = new TrackingDelegate(makeDataStructure())
    val p0 = Array(1.0, 2.0)
    val p1 = Array(2.0, 1.0)
    ds.addQueryPoint(p0)
    ds.expectChange(p0, 0)
    ds.expectNoOtherChange()
    ds.addDataPoint(p1, 7)
    ds.expectNoOtherChange()
  }

  @Test
  def oneDataOneQueryNonDominated(): Unit = {
    val ds = new TrackingDelegate(makeDataStructure())
    val p0 = Array(1.0, 2.0)
    val p1 = Array(2.0, 1.0)
    ds.addDataPoint(p1, 7)
    ds.expectNoOtherChange()
    ds.addQueryPoint(p0)
    ds.expectChange(p0, 0)
    ds.expectNoOtherChange()
  }

  @Test
  def oneQueryOneDataDominated(): Unit = {
    val ds = new TrackingDelegate(makeDataStructure())
    val p0 = Array(2.0, 2.0)
    val p1 = Array(1.0, 1.0)
    ds.addQueryPoint(p0)
    ds.expectChange(p0, 0)
    ds.expectNoOtherChange()
    ds.addDataPoint(p1, 7)
    ds.expectChange(p0, 7)
    ds.expectNoOtherChange()
  }

  @Test
  def oneDataOneQueryDominated(): Unit = {
    val ds = new TrackingDelegate(makeDataStructure())
    val p0 = Array(2.0, 2.0)
    val p1 = Array(1.0, 1.0)
    ds.addDataPoint(p1, 7)
    ds.expectNoOtherChange()
    ds.addQueryPoint(p0)
    ds.expectChange(p0, 7)
    ds.expectNoOtherChange()
  }

  @Test
  def smokeTest(): Unit = {
    case class Data(point: Array[Double], value: Int)
    case class Query(point: Array[Double]) {
      var realValue: Int = 0
      var expectedValue: Int = 0

      def updateWithNewData(d: Data): Unit = {
        if (Dominance.strict(d.point, point)) {
          expectedValue += d.value
        }
      }

      def validate(dim: Int, idx: Int): Unit = {
        Assert.assertEquals(s"dim = $dim, idx = $idx", expectedValue, realValue)
      }
    }

    val tracker = new NoUpdateIncrementalOrthantSearch.UpdateTracker[Int, Query] {
      override def valueChanged(value: Int, identifier: Query): Unit = {
        identifier.realValue = value
      }
    }

    val rng = new Random(8245435464734641L)

    for (dim <- 1 to 6) {
      val dataPoints = new ArrayBuffer[Data]()
      val queryPoints = new ArrayBuffer[Query]()
      val ds = makeDataStructure()

      for (j <- 0 to 1000) {
        if (rng.nextBoolean()) {
          val newDataPoint = Array.fill(dim)(rng.nextInt(10).toDouble)
          val newDataValue = rng.nextInt(623524352) - 383253461
          val d = Data(newDataPoint, newDataValue)
          dataPoints += d
          for (q <- queryPoints) q.updateWithNewData(d)
          ds.addDataPoint(newDataPoint, newDataValue)
          queryPoints.foreach(_.validate(dim, j))
        } else {
          val newQueryPoint = Array.fill(dim)(rng.nextInt(10).toDouble)
          val q = Query(newQueryPoint)
          queryPoints += q
          for (d <- dataPoints) q.updateWithNewData(d)
          ds.addQueryPoint(newQueryPoint, tracker, q)
          queryPoints.foreach(_.validate(dim, j))
        }
      }
    }
  }
}

object HandCraftedTests {
  class PlainArrayTests extends HandCraftedTests {
    override def makeDataStructure(): NoUpdateIncrementalOrthantSearch[Int] = new PlainArray[Int]()
  }
  class SimpleKDTests extends HandCraftedTests {
    override def makeDataStructure(): NoUpdateIncrementalOrthantSearch[Int] = new SimpleKD[Int]()
  }
}
