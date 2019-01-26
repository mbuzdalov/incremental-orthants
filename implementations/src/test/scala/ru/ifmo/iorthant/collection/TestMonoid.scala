package ru.ifmo.iorthant.collection

object TestMonoid extends Monoid[Int] {
  override def zero: Int = 0
  override def plus(a0: Int, a1: Int): Int = a0 + a1
}
