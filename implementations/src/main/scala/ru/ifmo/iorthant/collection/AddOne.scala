package ru.ifmo.iorthant.collection

trait AddOne[@specialized(Specialization.defaultSet) T] {
  type Handle
  def add(point: Array[Double], value: T): Handle
}
