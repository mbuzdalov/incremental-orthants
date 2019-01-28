package ru.ifmo.iorthant.noq2d

import java.util.Random

import scala.collection.mutable.ArrayBuffer

import org.junit.{Assert, Test}

import ru.ifmo.iorthant.util.Dominance

abstract class Tests {
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
    class Data(val point: Array[Double], val value: Int)
    class Query(val point: Array[Double]) {
      var realValue: Int = 0
      var expectedValue: Int = 0

      def addData(d: Data): Unit = {
        if (Dominance.strict(d.point, point)) {
          expectedValue += d.value
        }
      }

      def removeData(d: Data): Unit = {
        if (Dominance.strict(d.point, point)) {
          expectedValue -= d.value
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
      val ds = makeDataStructure()

      class DataEx(point: Array[Double], value: Int, val handle: ds.DataPointHandle) extends Data(point, value)
      class QueryEx(point: Array[Double]) extends Query(point) {
        val handle: ds.QueryPointHandle = ds.addQueryPoint(point, tracker, this)
      }

      val dataPoints = new ArrayBuffer[DataEx]()
      val queryPoints = new ArrayBuffer[QueryEx]()

      for (j <- 0 to 1000) {
        if (rng.nextBoolean()) {
          if (dataPoints.nonEmpty && rng.nextInt(10) < 3) {
            val d = dataPoints(rng.nextInt(dataPoints.size))
            dataPoints -= d
            for (q <- queryPoints) q.removeData(d)
            ds.removeDataPoint(d.handle)
          } else {
            val newDataPoint = Array.fill(dim)(rng.nextInt(10).toDouble)
            val newDataValue = rng.nextInt(623524352) - 383253461
            val d = new DataEx(newDataPoint, newDataValue, ds.addDataPoint(newDataPoint, newDataValue))
            dataPoints += d
            for (q <- queryPoints) q.addData(d)
          }
        } else {
          if (queryPoints.nonEmpty && rng.nextInt(10) < 3) {
            val q = queryPoints(rng.nextInt(queryPoints.size))
            queryPoints -= q
            ds.removeQueryPoint(q.handle)
          } else {
            val newQueryPoint = Array.fill(dim)(rng.nextInt(10).toDouble)
            val q = new QueryEx(newQueryPoint) // adds itself to the data structure
            queryPoints += q
            for (d <- dataPoints) q.addData(d)
          }
        }
        queryPoints.foreach(_.validate(dim, j))
      }
    }
  }
}

object Tests {
  class PlainArrayTests extends Tests {
    override def makeDataStructure(): NoUpdateIncrementalOrthantSearch[Int] = new PlainArray[Int]()
  }
  class SimpleKDTests extends Tests {
    override def makeDataStructure(): NoUpdateIncrementalOrthantSearch[Int] = new SimpleKD[Int](0)
  }
  class SimpleKDBoundedTests extends Tests {
    override def makeDataStructure(): NoUpdateIncrementalOrthantSearch[Int] = new SimpleKDBounded[Int](0)
  }
}
