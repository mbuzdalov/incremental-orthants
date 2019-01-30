package ru.ifmo.iorthant.noq2d

import ru.ifmo.iorthant.util.{HasMinus, Specialization}

trait NoUpdateIncrementalOrthantSearch[@specialized(Specialization.defaultSet) T] {
  type DataPointHandle
  type QueryPointHandle

  def newDataPointHandleArray(n: Int): Array[DataPointHandle]
  def newQueryPointHandleArray(n: Int): Array[QueryPointHandle]

  def addDataPoint(point: Array[Double], value: T): DataPointHandle
  def addQueryPoint[@specialized(Specialization.defaultSet) I](point: Array[Double],
                       tracker: NoUpdateIncrementalOrthantSearch.UpdateTracker[T, I],
                       identifier: I): QueryPointHandle

  def makeQuery(point: Array[Double]): T

  def removeDataPoint(handle: DataPointHandle)(implicit hm: HasMinus[T]): Unit
  def removeQueryPoint(handle: QueryPointHandle): Unit
}

object NoUpdateIncrementalOrthantSearch {
  trait UpdateTracker[@specialized(Specialization.defaultSet) T, @specialized(Specialization.defaultSet) I] {
    def valueChanged(oldValue: T, newValue: T, identifier: I): Unit
  }
}
