package ru.ifmo.iorthant.noq2d

import ru.ifmo.iorthant.noq2d.NoUpdateIncrementalOrthantSearch.{UpdateTracker => Tracker}
import ru.ifmo.iorthant.util.{HasNegation, Monoid}
import ru.ifmo.iorthant.util.Specialization.defaultSet

trait QueryWrapper[@specialized(defaultSet) T] {
  def point: Array[Double]
  def plus(v: T)(implicit m: Monoid[T]): Unit
  def minus(v: T)(implicit m: HasNegation[T]): Unit
}

object QueryWrapper {
  class Tracking[@specialized(defaultSet) T, @specialized(defaultSet) I]
                (val point: Array[Double], value: T, tracker: Tracker[T, I], identifier: I)
                (implicit m: Monoid[T])
    extends QueryWrapper[T]
  {
    tracker.valueChanged(value, identifier)

    def plus(v: T)(implicit m: Monoid[T]): Unit = {
      tracker.valueChanged(v, identifier)
    }

    def minus(v: T)(implicit m: HasNegation[T]): Unit = {
      tracker.valueChanged(m.negate(v), identifier)
    }
  }
}
