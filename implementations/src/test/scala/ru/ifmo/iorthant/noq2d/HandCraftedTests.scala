package ru.ifmo.iorthant.noq2d

import org.junit.Test

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
}

object HandCraftedTests {
  class PlainArrayTests extends HandCraftedTests {
    override def makeDataStructure(): NoUpdateIncrementalOrthantSearch[Int] = new PlainArray[Int]()
  }
}
