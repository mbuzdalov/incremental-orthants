package ru.ifmo.iorthant.collection

trait GetPrefix[@specialized(Specialization.defaultSet) T] {
  def getOnPrefix(point: Array[Double]): T
}
