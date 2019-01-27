package ru.ifmo.iorthant.noq2d

import scala.reflect.ClassTag

import ru.ifmo.iorthant.util.{HasMinus, Specialization}

trait NoUpdateIncrementalOrthantSearch[@specialized(Specialization.defaultSet) T] {
  type DataPointHandle
  type QueryPointHandle

  implicit def dataPointHandleClassTag: ClassTag[DataPointHandle]
  implicit def queryPointHandleClassTag: ClassTag[QueryPointHandle]

  def addDataPoint(point: Array[Double], value: T): DataPointHandle
  def addQueryPoint[I](point: Array[Double],
                       tracker: NoUpdateIncrementalOrthantSearch.UpdateTracker[T, I],
                       identifier: I): QueryPointHandle

  def makeQuery(point: Array[Double]): T

  def removeDataPoint(handle: DataPointHandle)(implicit hm: HasMinus[T]): Unit
  def removeQueryPoint(handle: QueryPointHandle): Unit
}

object NoUpdateIncrementalOrthantSearch {
  trait UpdateTracker[@specialized(Specialization.defaultSet) T, I] {
    def valueChanged(value: T, identifier: I): Unit
  }
}
