package ru.ifmo.iorthant.collection

trait GetOne[@specialized(Specialization.defaultSet) T] { self: AddOne[T] =>
  def get(pointHandle: Handle): T
}
