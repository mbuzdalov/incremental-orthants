package ru.ifmo.iorthant.noq2d

import ru.ifmo.iorthant.noq2d.NoUpdateIncrementalOrthantSearch.{UpdateTracker => Tracker}
import ru.ifmo.iorthant.util.{HasNegation, Monoid}
import ru.ifmo.iorthant.util.Specialization.defaultSet

trait QueryWrapper[@specialized(defaultSet) T]:
  def point: Array[Double]
  def plus(v: T)(using Monoid[T]): Unit
  def minus(v: T)(using HasNegation[T]): Unit

object QueryWrapper:
  class Tracking[@specialized(defaultSet) T, @specialized(defaultSet) I]
                (val point: Array[Double], value: T, tracker: Tracker[T, I], identifier: I)
  extends QueryWrapper[T]:
    tracker.valueChanged(value, identifier)

    override def plus(v: T)(using Monoid[T]): Unit =
      tracker.valueChanged(v, identifier)

    override def minus(v: T)(using m: HasNegation[T]): Unit =
      tracker.valueChanged(m.negate(v), identifier)
