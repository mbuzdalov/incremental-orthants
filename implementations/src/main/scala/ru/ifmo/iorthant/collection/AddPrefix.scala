package ru.ifmo.iorthant.collection

trait AddPrefix[@specialized(Specialization.defaultSet) T] {
  def addToPrefix(point: Array[Double], value: T): Unit
}
