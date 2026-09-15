package ru.ifmo.iorthant.noq2d

import scala.collection.mutable.ArrayBuffer

import ru.ifmo.iorthant.util.Syntax._
import ru.ifmo.iorthant.util.{Dominance, HasNegation, Monoid, Specialization}

class PlainArray[@specialized(Specialization.defaultSet) T]()(using m: Monoid[T])
  extends NoUpdateIncrementalOrthantSearch[T]:

  override type DataPointHandle = DataWrapper[T]
  override type QueryPointHandle = QueryWrapper[T]

  override def newDataPointHandleArray(n: Int): Array[DataPointHandle] = Array.ofDim(n)
  override def newQueryPointHandleArray(n: Int): Array[QueryPointHandle] = Array.ofDim(n)

  private val dataPoints = new ArrayBuffer[DataPointHandle]()
  private val queryPoints = new ArrayBuffer[QueryPointHandle]()

  override def addDataPoint(point: Array[Double], value: T): DataPointHandle =
    for q <- queryPoints do
      if Dominance.strict(point, q.point) 
      then q.plus(value)
    DataWrapper[T](point, value).addTo(dataPoints)

  override def addQueryPoint[@specialized(Specialization.defaultSet) I](point: Array[Double],
                             tracker: NoUpdateIncrementalOrthantSearch.UpdateTracker[T, I],
                             identifier: I): QueryPointHandle =
    QueryWrapper.Tracking[T, I](point, makeQuery(point), tracker, identifier).addTo(queryPoints)

  override def makeQuery(point: Array[Double]): T =
    var sum = m.zero
    for d <- dataPoints do
      if Dominance.strict(d.point, point) then
        sum = m.plus(sum, d.value)
    sum

  override def removeDataPoint(handle: DataPointHandle)(using HasNegation[T]): Unit = 
    val point = handle.point
    val value = handle.value
    for q <- queryPoints do
      if Dominance.strict(point, q.point) then
        q.minus(value)
    dataPoints -= handle

  override def removeQueryPoint(handle: QueryPointHandle): Unit = queryPoints -= handle
