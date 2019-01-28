package ru.ifmo.iorthant.util

import java.util.concurrent.ThreadLocalRandom

import scala.collection.mutable.ArrayBuffer

abstract class KDTree[D] {
  final def add(point: Array[Double], data: D): KDTree[D] = add(point, data, 0)
  protected def add(point: Array[Double], data: D, index: Int): KDTree[D]
  def remove(point: Array[Double], data: D): KDTree[D]
  def forDominating(ctx: KDTree.TraverseContext[D]): Unit
  def isEmpty: Boolean
}

object KDTree {
  trait TraverseContext[-D] {
    def point: Array[Double]
    def dominates(lhs: Array[Double], rhs: Array[Double]): Boolean
    def update(data: D): Unit
  }

  private def chooseDifferentCoordinate(a: Array[Double], b: Array[Double], first: Int): Int = {
    if (a(first) != b(first)) {
      first
    } else {
      chooseDifferentCoordinate(a, b, (first + 1) % a.length)
    }
  }

  private def createSeparator(a: Double, b: Double): Double = {
    a + (b - a) * ThreadLocalRandom.current().nextDouble()
  }

  class Empty[D] extends KDTree[D] {
    override def add(point: Array[Double], data: D, index: Int): KDTree[D] = new Leaf(point, data)
    override def forDominating(ctx: TraverseContext[D]): Unit = {}
    override def remove(point: Array[Double], data: D): KDTree[D] = {
      throw new IllegalArgumentException("No point can be in an empty KDTree")
    }
    override def isEmpty: Boolean = true
  }

  private val emptyInstance = new Empty[Nothing]
  def empty[D]: Empty[D] = emptyInstance.asInstanceOf[Empty[D]]

  class Branch[D](val index: Int, val value: Double, var left: KDTree[D], var right: KDTree[D]) extends KDTree[D] {
    override def add(point: Array[Double], data: D, index: Int): KDTree[D] = {
      if (point(this.index) <= value) {
        left = left.add(point, data, (this.index + 1) % point.length)
        this
      } else {
        right = right.add(point, data, (this.index + 1) % point.length)
        this
      }
    }

    override def forDominating(ctx: TraverseContext[D]): Unit = {
      left.forDominating(ctx)
      if (ctx.point(index) > value) {
        right.forDominating(ctx)
      }
    }

    override def remove(point: Array[Double], data: D): KDTree[D] = {
      if (point(index) <= value) {
        left = left.remove(point, data)
        if (left.isEmpty) right else this
      } else {
        right = right.remove(point, data)
        if (right.isEmpty) left else this
      }
    }

    override def isEmpty: Boolean = false
  }

  class Leaf[D](val point: Array[Double], val data: ArrayBuffer[D]) extends KDTree[D] {
    def this(point: Array[Double], data: D) {
      this(point, new ArrayBuffer[D](1))
      this.data += data
    }

    override def add(point: Array[Double], data: D, index: Int): KDTree[D] = {
      if (Arrays.equal(this.point, point)) {
        this.data += data
        this
      } else {
        val idx = chooseDifferentCoordinate(point, this.point, index)
        val thisV = this.point(idx)
        val thatV = point(idx)
        val sep = createSeparator(thisV, thatV)
        val that = new Leaf(point, data)
        if (thisV < thatV) {
          new Branch(idx, sep, this, that)
        } else {
          new Branch(idx, sep, that, this)
        }
      }
    }

    override def forDominating(ctx: TraverseContext[D]): Unit = {
      if (ctx.dominates(point, ctx.point)) {
        for (d <- data) ctx.update(d)
      }
    }

    override def remove(point: Array[Double], data: D): KDTree[D] = {
      val idx = this.data.indexOf(data)
      this.data.remove(idx)
      if (this.data.isEmpty) empty else this
    }

    override def isEmpty: Boolean = false
  }
}
