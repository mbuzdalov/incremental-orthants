package ru.ifmo.iorthant.noq2d

import ru.ifmo.iorthant.util.{HasMinus, Specialization}

trait NoUpdateIncrementalOrthantSearch[@specialized(Specialization.defaultSet) T] {
  type DataPointHandle
  type QueryPointHandle

  def addDataPoint(point: Array[Double], value: T): DataPointHandle
  def addQueryPoint(point: Array[Double], tracker: NoUpdateIncrementalOrthantSearch.UpdateTracker[T]): QueryPointHandle

  def makeQuery(point: Array[Double]): T

  def removeDataPoint(handle: DataPointHandle)(implicit hm: HasMinus[T]): Unit
  def removeQueryPoint(handle: QueryPointHandle): Unit
}

object NoUpdateIncrementalOrthantSearch {
  trait UpdateTracker[@specialized(Specialization.defaultSet) T] {
    def valueChanged(point: Array[Double], value: T): Unit
  }
}
