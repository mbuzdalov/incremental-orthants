package ru.ifmo.iorthant.util

object Dominance {
  def nonStrict(lhs: Array[Double], rhs: Array[Double]): Boolean = nonStrictImpl(lhs, rhs, lhs.length - 1, 0)
  def strict(lhs: Array[Double], rhs: Array[Double]): Boolean = nonStrict(lhs, rhs) && !Arrays.equal(lhs, rhs)
  def partial(lhs: Array[Double], rhs: Array[Double], minNonStrict: Int): Boolean = {
    nonStrictImpl(lhs, rhs, lhs.length - 1, minNonStrict) &&
      (if (minNonStrict == 0) !Arrays.equal(lhs, rhs) else strictImpl(lhs, rhs, minNonStrict - 1, 0))
  }

  private[this] def nonStrictImpl(lhs: Array[Double], rhs: Array[Double], i: Int, iMin: Int): Boolean = {
    lhs(i) <= rhs(i) && (i == iMin || nonStrictImpl(lhs, rhs, i - 1, iMin))
  }
  private[this] def strictImpl(lhs: Array[Double], rhs: Array[Double], i: Int, iMin: Int): Boolean = {
    lhs(i) < rhs(i) && (i == iMin || strictImpl(lhs, rhs, i - 1, iMin))
  }
}
