package ru.ifmo.iorthant.util

object Dominance {
  private[this] def nonStrictImpl(lhs: Array[Double], rhs: Array[Double], i: Int): Boolean = {
    lhs(i) <= rhs(i) && (i == 0 || nonStrictImpl(lhs, rhs, i - 1))
  }

  def equal(lhs: Array[Double], rhs: Array[Double]): Boolean = {
    def impl(idx: Int): Boolean = idx < 0 || lhs(idx) == rhs(idx) && impl(idx - 1)
    lhs.length == rhs.length && impl(lhs.length - 1)
  }

  def nonStrict(lhs: Array[Double], rhs: Array[Double]): Boolean = nonStrictImpl(lhs, rhs, math.max(lhs.length, rhs.length) - 1)
  def strict(lhs: Array[Double], rhs: Array[Double]): Boolean = !equal(lhs, rhs) && nonStrict(lhs, rhs)

  def negate(a: Array[Double]): Array[Double] = {
    var i = a.length
    val rv = new Array[Double](i)
    while (i > 0) {
      i -= 1
      rv(i) = -a(i)
    }
    rv
  }
}
