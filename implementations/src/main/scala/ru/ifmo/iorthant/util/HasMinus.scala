package ru.ifmo.iorthant.util

trait HasMinus[@specialized(Specialization.defaultSet) T] {
  def minus(lhs: T, rhs: T): T
}
