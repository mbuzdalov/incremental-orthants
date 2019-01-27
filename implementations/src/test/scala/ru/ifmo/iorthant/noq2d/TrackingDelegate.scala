package ru.ifmo.iorthant.noq2d

import scala.collection.mutable.ArrayBuffer

import ru.ifmo.iorthant.util.{HasMinus, Specialization}

class TrackingDelegate[@specialized(Specialization.defaultSet) T](val impl: NoUpdateIncrementalOrthantSearch[T])
  extends NoUpdateIncrementalOrthantSearch[T] with NoUpdateIncrementalOrthantSearch.UpdateTracker[T, Array[Double]] {
  private val myEvents = new ArrayBuffer[TrackingDelegate.ValueChanged[T]]()

  override type DataPointHandle = impl.DataPointHandle
  override type QueryPointHandle = impl.QueryPointHandle

  override def newDataPointHandleArray(n: Int): Array[DataPointHandle] = impl.newDataPointHandleArray(n)
  override def newQueryPointHandleArray(n: Int): Array[QueryPointHandle] = impl.newQueryPointHandleArray(n)

  override def addDataPoint(point: Array[Double], value: T): DataPointHandle = impl.addDataPoint(point, value)

  def addQueryPoint(point: Array[Double]): QueryPointHandle = impl.addQueryPoint(point, this, point)

  override def addQueryPoint[I](point: Array[Double],
                                tracker: NoUpdateIncrementalOrthantSearch.UpdateTracker[T, I],
                                identifier: I): QueryPointHandle = addQueryPoint(point)

  override def makeQuery(point: Array[Double]): T = impl.makeQuery(point)

  override def removeDataPoint(handle: DataPointHandle)
                              (implicit hm: HasMinus[T]): Unit = impl.removeDataPoint(handle)

  override def removeQueryPoint(handle: QueryPointHandle): Unit = impl.removeQueryPoint(handle)

  override def valueChanged(value: T, point: Array[Double]): Unit = myEvents += TrackingDelegate.ValueChanged(point, value)

  def expectChange(point: Array[Double], value: T): Unit = {
    val vc = TrackingDelegate.ValueChanged(point, value)
    val index = myEvents.indexOf(vc)
    if (index == -1) {
      throw new IllegalStateException(s"The change (${point.mkString(",")}) => $value should have been there")
    }
    myEvents.remove(index)
  }

  def expectNoOtherChange(): Unit = {
    if (myEvents.nonEmpty) {
      val TrackingDelegate.ValueChanged(point, value) = myEvents.head
      throw new IllegalArgumentException(s"There are unexpected changes; the first is (${point.mkString(",")}) => $value")
    }
  }
}

object TrackingDelegate {
  final case class ValueChanged[@specialized(Specialization.defaultSet) T](point: Array[Double], value: T)
}
