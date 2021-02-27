package ru.ifmo.iorthant.util

import scala.annotation.tailrec

object Arrays {
  final def equal(lhs: Array[Double], rhs: Array[Double]): Boolean = {
    @tailrec
    def impl(idx: Int): Boolean = idx < 0 || lhs(idx) == rhs(idx) && impl(idx - 1)
    impl(lhs.length - 1)
  }
}
