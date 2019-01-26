package ru.ifmo.iorthant.util

trait Monoid[@specialized(Specialization.defaultSet) T] {
  def zero: T
  def plus(lhs: T, rhs: T): T
}
