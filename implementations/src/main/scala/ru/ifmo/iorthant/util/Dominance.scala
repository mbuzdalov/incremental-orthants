package ru.ifmo.iorthant.util

import java.util

object Dominance {
  private[this] def nonStrictImpl(lhs: Array[Double], rhs: Array[Double], i: Int): Boolean = {
    lhs(i) <= rhs(i) && (i == 0 || nonStrictImpl(lhs, rhs, i - 1))
  }

  def nonStrict(lhs: Array[Double], rhs: Array[Double]): Boolean = nonStrictImpl(lhs, rhs, math.max(lhs.length, rhs.length) - 1)
  def strict(lhs: Array[Double], rhs: Array[Double]): Boolean = !util.Arrays.equals(lhs, rhs) && nonStrict(lhs, rhs)
}
