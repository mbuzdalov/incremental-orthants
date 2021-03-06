package ru.ifmo.iorthant.util.kd

import java.util.concurrent.ThreadLocalRandom

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

import ru.ifmo.iorthant.util.{Arrays, KDTree}
import ru.ifmo.iorthant.util.KDTree.TraverseContext

/*
 * Increasing and decreasing trees are different only in few locations (sign changed). This is for performance. Sorry.
 */
object DecreasingTree {
  @tailrec
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

  private class Empty[D] extends KDTree[D] {
    override protected[util] def addImpl(point: Array[Double], data: D, index: Int): KDTree[D] = new Leaf(point, data)
    override protected[util] def forDominatingImpl(ctx: TraverseContext[D], mask: Int): Unit = {}
    override def remove(point: Array[Double], data: D): KDTree[D] = {
      throw new IllegalArgumentException("No point can be in an empty KDTree")
    }
    override def isEmpty: Boolean = true
  }

  private val emptyInstance = new Empty[Nothing]

  def empty[D]: KDTree[D] = emptyInstance.asInstanceOf[Empty[D]]

  private class Branch[D](index: Int,
                          value: Double,
                          private[this] var left: KDTree[D],
                          private[this] var right: KDTree[D]) extends KDTree[D] {
    override protected[util] def addImpl(point: Array[Double], data: D, index: Int): KDTree[D] = {
      if (point(this.index) >= value) {
        left = left.addImpl(point, data, (this.index + 1) % point.length)
        this
      } else {
        right = right.addImpl(point, data, (this.index + 1) % point.length)
        this
      }
    }

    override protected[util] def forDominatingImpl(ctx: TraverseContext[D], mask: Int): Unit = {
      val bit = 1 << index
      if ((mask & bit) == 0) {
        left.forDominatingImpl(ctx, mask)
        right.forDominatingImpl(ctx, mask)
      } else {
        if (ctx.point(index) < value) {
          left.forDominatingImpl(ctx, mask & ~bit)
          right.forDominatingImpl(ctx, mask)
        } else {
          left.forDominatingImpl(ctx, mask)
        }
      }
    }

    override def remove(point: Array[Double], data: D): KDTree[D] = {
      if (point(index) >= value) {
        left = left.remove(point, data)
        if (left.isEmpty) right else this
      } else {
        right = right.remove(point, data)
        if (right.isEmpty) left else this
      }
    }

    override def isEmpty: Boolean = false
  }

  private class Leaf[D](point: Array[Double], private[this] var data0: D) extends KDTree[D] {
    private[this] var dataMore: ArrayBuffer[D] = _

    override protected[util] def addImpl(point: Array[Double], data: D, index: Int): KDTree[D] = {
      if (Arrays.equal(this.point, point)) {
        if (dataMore == null) {
          dataMore = new ArrayBuffer[D](2)
        }
        dataMore += data
        this
      } else {
        val idx = chooseDifferentCoordinate(point, this.point, index)
        val thisV = this.point(idx)
        val thatV = point(idx)
        val sep = createSeparator(thisV, thatV)
        val that = new Leaf(point, data)
        if (thisV > thatV) {
          new Branch(idx, sep, this, that)
        } else {
          new Branch(idx, sep, that, this)
        }
      }
    }

    override protected[util] def forDominatingImpl(ctx: TraverseContext[D], mask: Int): Unit = {
      if (mask == 0 || ctx.isDominatedBy(point)) {
        ctx.update(data0)
        if (dataMore != null) {
          dataMore.foreach(ctx.update)
        }
      }
    }

    override def remove(point: Array[Double], data: D): KDTree[D] = {
      if (data0 == data) {
        if (dataMore == null) {
          empty
        } else {
          val lastIndex = dataMore.size - 1
          data0 = dataMore(lastIndex)
          if (lastIndex == 0) {
            dataMore = null
          } else {
            dataMore.remove(lastIndex)
          }
          this
        }
      } else {
        val idx = dataMore.indexOf(data)
        dataMore.remove(idx)
        if (dataMore.isEmpty) {
          dataMore = null
        }
        this
      }
    }

    override def isEmpty: Boolean = false
  }
}
