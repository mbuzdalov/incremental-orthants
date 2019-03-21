package ru.ifmo.iorthant.noq2d

import ru.ifmo.iorthant.util.{HasNegation, Monoid}

object DefaultIntMonoid extends Monoid[Int] with HasNegation[Int] {
  override def zero: Int = 0
  override def plus(lhs: Int, rhs: Int): Int = lhs + rhs
  override def negate(arg: Int): Int = -arg
}
