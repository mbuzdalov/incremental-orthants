package ru.ifmo.iorthant.util

abstract class KDTree[D] {
  final def add(point: Array[Double], data: D): KDTree[D] = addImpl(point, data, 0)
  final def forDominating(ctx: KDTree.TraverseContext[D]): Unit = forDominatingImpl(ctx, Bits.intMask(ctx.point.length))

  def remove(point: Array[Double], data: D): KDTree[D]
  def isEmpty: Boolean

  protected[util] def addImpl(point: Array[Double], data: D, index: Int): KDTree[D]
  protected[util] def forDominatingImpl(ctx: KDTree.TraverseContext[D], mask: Int): Unit
}

object KDTree {
  trait TraverseContext[-D] {
    def point: Array[Double]
    def isDominatedBy(point: Array[Double]): Boolean
    def update(data: D): Unit
  }
}
