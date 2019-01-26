package ru.ifmo.iorthant.noq2d

import ru.ifmo.iorthant.util.{HasMinus, Monoid}

object DefaultIntMonoid extends Monoid[Int] with HasMinus[Int] {
  override def zero: Int = 0
  override def plus(lhs: Int, rhs: Int): Int = lhs + rhs
  override def minus(lhs: Int, rhs: Int): Int = lhs - rhs
}
