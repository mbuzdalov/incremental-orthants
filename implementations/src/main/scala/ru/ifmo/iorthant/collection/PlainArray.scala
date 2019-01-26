package ru.ifmo.iorthant.collection

import scala.collection.mutable.ArrayBuffer

import ru.ifmo.iorthant.util.Dominance

class PlainArray[@specialized(Specialization.defaultSet) T](implicit m: Monoid[T])
  extends AddOne[T] with AddPrefix[T] with GetOne[T] with GetPrefix[T] {

  override type Handle = PlainArray.Wrapper[T]

  private val points = new ArrayBuffer[PlainArray.Wrapper[T]]

  override def add(point: Array[Double], value: T): Handle = {
    val handle = new PlainArray.Wrapper[T](point, value)
    points += handle
    handle
  }

  override def addToPrefix(point: Array[Double], value: T): Unit = {
    points.foreach(p => if (Dominance.strict(p.point, point)) p.value = m.plus(p.value, value))
  }

  override def get(pointHandle: Handle): T = pointHandle.value

  override def getOnPrefix(point: Array[Double]): T = {
    def process(index: Int, value: T): T = if (index < 0) value else {
      val w = points(index)
      process(index - 1, if (Dominance.strict(w.point, point)) m.plus(value, w.value) else value)
    }
    process(points.size - 1, m.zero)
  }
}

object PlainArray {
  class Wrapper[@specialized(Specialization.defaultSet) T](val point: Array[Double], var value: T)
}
