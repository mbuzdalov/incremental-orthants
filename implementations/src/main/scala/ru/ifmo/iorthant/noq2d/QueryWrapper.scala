package ru.ifmo.iorthant.noq2d

import ru.ifmo.iorthant.noq2d.NoUpdateIncrementalOrthantSearch.{UpdateTracker => Tracker}
import ru.ifmo.iorthant.util.{HasMinus, Monoid}
import ru.ifmo.iorthant.util.Specialization.defaultSet

trait QueryWrapper[@specialized(defaultSet) T] {
  def point: Array[Double]
  def plus(v: T)(implicit m: Monoid[T]): Unit
  def minus(v: T)(implicit m: HasMinus[T]): Unit
}

object QueryWrapper {
  class Tracking[@specialized(defaultSet) T, @specialized(defaultSet) I]
                (val point: Array[Double], private[this] var value: T, tracker: Tracker[T, I], identifier: I)
                (implicit m: Monoid[T])
    extends QueryWrapper[T]
  {
    tracker.valueChanged(m.zero, value, identifier)

    def plus(v: T)(implicit m: Monoid[T]): Unit = {
      val old = value
      value = m.plus(value, v)
      tracker.valueChanged(old, value, identifier)
    }

    def minus(v: T)(implicit m: HasMinus[T]): Unit = {
      val old = value
      value = m.minus(value, v)
      tracker.valueChanged(old, value, identifier)
    }
  }
}
