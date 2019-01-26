package ru.ifmo.iorthant.noq2d

import scala.collection.mutable.ArrayBuffer

import ru.ifmo.iorthant.util.{Dominance, HasMinus, Monoid, Specialization}
import ru.ifmo.iorthant.util.Syntax._

class PlainArray[@specialized(Specialization.defaultSet) T](implicit m: Monoid[T])
  extends NoUpdateIncrementalOrthantSearch[T] {
  import PlainArray._

  override type DataPointHandle = DataWrapper[T]
  override type QueryPointHandle = QueryWrapper[T]

  private val dataPoints = new ArrayBuffer[DataPointHandle]()
  private val queryPoints = new ArrayBuffer[QueryPointHandle]()

  override def addDataPoint(point: Array[Double], value: T): DataPointHandle = {
    for (q <- queryPoints) {
      if (Dominance.strict(point, q.point)) {
        q.plus(value)
      }
    }
    new DataWrapper[T](point, value).addTo(dataPoints)
  }

  override def addQueryPoint(point: Array[Double],
                             tracker: NoUpdateIncrementalOrthantSearch.UpdateTracker[T]): QueryPointHandle = {
    new QueryWrapper[T](point, makeQuery(point), tracker).addTo(queryPoints)
  }

  override def makeQuery(point: Array[Double]): T = {
    var sum = m.zero
    for (d <- dataPoints) {
      if (Dominance.strict(d.point, point)) {
        sum = m.plus(sum, d.value)
      }
    }
    sum
  }

  override def removeDataPoint(handle: DataPointHandle)
                              (implicit hm: HasMinus[T]): Unit = {
    val point = handle.point
    val value = handle.value
    for (q <- queryPoints) {
      if (Dominance.strict(point, q.point)) {
        q.minus(value)
      }
    }
    dataPoints -= handle
  }

  override def removeQueryPoint(handle: QueryPointHandle): Unit = queryPoints -= handle
}

object PlainArray {
  class DataWrapper[@specialized(Specialization.defaultSet) T](val point: Array[Double],
                                                               val value: T)
  class QueryWrapper[@specialized(Specialization.defaultSet) T](val point: Array[Double],
                                                                private var value: T,
                                                                private val tracker: NoUpdateIncrementalOrthantSearch.UpdateTracker[T]) {
    tracker.valueChanged(point, value)

    def plus(v: T)(implicit m: Monoid[T]): Unit = {
      value = m.plus(value, v)
      tracker.valueChanged(point, value)
    }

    def minus(v: T)(implicit m: HasMinus[T]): Unit = {
      value = m.minus(value, v)
      tracker.valueChanged(point, value)
    }
  }
}
