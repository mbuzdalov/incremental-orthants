package ru.ifmo.iorthant.util

trait HasNegation[@specialized(Specialization.defaultSet) T] {
  def negate(arg: T): T
}
