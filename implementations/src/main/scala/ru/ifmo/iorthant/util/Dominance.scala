package ru.ifmo.iorthant.util

import scala.annotation.tailrec

object Dominance {
  def nonStrict(lhs: Array[Double], rhs: Array[Double]): Boolean = nonStrictImpl(lhs, rhs, 0, lhs.length)
  def strict(lhs: Array[Double], rhs: Array[Double]): Boolean = nonStrict(lhs, rhs) && !Arrays.equal(lhs, rhs)
  def partial(lhs: Array[Double], rhs: Array[Double], minNonStrict: Int): Boolean = {
    if (minNonStrict == 0) {
      nonStrictImpl(lhs, rhs, 0, lhs.length) && !Arrays.equal(lhs, rhs)
    } else {
      strictImpl(lhs, rhs, 0, minNonStrict) && nonStrictImpl(lhs, rhs, minNonStrict, lhs.length)
    }
  }

  @tailrec
  private def nonStrictImpl(lhs: Array[Double], rhs: Array[Double], i: Int, iMax: Int): Boolean = {
    i == iMax || lhs(i) <= rhs(i) && nonStrictImpl(lhs, rhs, i + 1, iMax)
  }

  @tailrec
  private def strictImpl(lhs: Array[Double], rhs: Array[Double], i: Int, iMax: Int): Boolean = {
    i == iMax || lhs(i) < rhs(i) && strictImpl(lhs, rhs, i + 1, iMax)
  }
}
