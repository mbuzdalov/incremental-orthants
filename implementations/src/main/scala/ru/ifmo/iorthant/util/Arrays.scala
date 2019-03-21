package ru.ifmo.iorthant.util

object Arrays {
  final def equal(lhs: Array[Double], rhs: Array[Double]): Boolean = {
    def impl(idx: Int): Boolean = idx < 0 || lhs(idx) == rhs(idx) && impl(idx - 1)
    impl(lhs.length - 1)
  }

  final def swap[@specialized(Double) T](array: Array[T], i1: Int, i2: Int): Unit = {
    val tmp = array(i1)
    array(i1) = array(i2)
    array(i2) = tmp
  }

  final def negate(a: Array[Double]): Array[Double] = {
    var i = a.length
    val rv = new Array[Double](i)
    while (i > 0) {
      i -= 1
      rv(i) = -a(i)
    }
    rv
  }
}
