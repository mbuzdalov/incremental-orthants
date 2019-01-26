package ru.ifmo.iorthant.collection

trait Monoid[@specialized(Specialization.defaultSet) T] {
  def zero: T
  def plus(a0: T, a1: T): T
}
