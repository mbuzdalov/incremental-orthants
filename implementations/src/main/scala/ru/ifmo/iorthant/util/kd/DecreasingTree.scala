package ru.ifmo.iorthant.util.kd

import java.util.concurrent.ThreadLocalRandom
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import ru.ifmo.iorthant.util.{Arrays, KDTree}
import ru.ifmo.iorthant.util.KDTree.TraverseContext

import scala.compiletime.uninitialized

/*
 * Increasing and decreasing trees are different only in few locations (sign changed). This is for performance. Sorry.
 */
object DecreasingTree:
  @tailrec
  private def chooseDifferentCoordinate(a: Array[Double], b: Array[Double], first: Int): Int =
    if a(first) != b(first) 
    then first
    else chooseDifferentCoordinate(a, b, (first + 1) % a.length)

  private def createSeparator(a: Double, b: Double): Double = a + (b - a) * ThreadLocalRandom.current().nextDouble()

  private class Empty[D] extends KDTree[D]:
    override protected[util] def addImpl(point: Array[Double], data: D, index: Int): KDTree[D] = Leaf(point, data)
    override protected[util] def forDominatingImpl(ctx: TraverseContext[D], mask: Int): Unit = {}
    override def remove(point: Array[Double], data: D): KDTree[D] =
      throw IllegalArgumentException("No point can be in an empty KDTree")
    override def isEmpty: Boolean = true

  private val emptyInstance = Empty[Nothing]()

  def empty[D]: KDTree[D] = emptyInstance.asInstanceOf[Empty[D]]

  private class Branch[D](index: Int,
                          value: Double,
                          private var left: KDTree[D],
                          private var right: KDTree[D]) extends KDTree[D]:
    override protected[util] def addImpl(point: Array[Double], data: D, index: Int): KDTree[D] =
      if point(this.index) >= value 
      then left = left.addImpl(point, data, (this.index + 1) % point.length)
      else right = right.addImpl(point, data, (this.index + 1) % point.length)
      this

    override protected[util] def forDominatingImpl(ctx: TraverseContext[D], mask: Int): Unit =
      val bit = 1 << index
      if (mask & bit) == 0 then
        left.forDominatingImpl(ctx, mask)
        right.forDominatingImpl(ctx, mask)
      else if ctx.point(index) < value then
        left.forDominatingImpl(ctx, mask & ~bit)
        right.forDominatingImpl(ctx, mask)
      else 
        left.forDominatingImpl(ctx, mask)

    override def remove(point: Array[Double], data: D): KDTree[D] =
      if point(index) >= value then
        left = left.remove(point, data)
        if left.isEmpty then right else this
      else
        right = right.remove(point, data)
        if right.isEmpty then left else this

    override def isEmpty: Boolean = false
  end Branch

  private class Leaf[D](point: Array[Double], private var data0: D) extends KDTree[D]:
    private var dataMore: ArrayBuffer[D] = uninitialized

    override protected[util] def addImpl(point: Array[Double], data: D, index: Int): KDTree[D] =
      if Arrays.equal(this.point, point) then
        if dataMore == null then dataMore = new ArrayBuffer[D](2)
        dataMore += data
        this
      else
        val idx = chooseDifferentCoordinate(point, this.point, index)
        val thisV = this.point(idx)
        val thatV = point(idx)
        val sep = createSeparator(thisV, thatV)
        val that = Leaf(point, data)
        if thisV > thatV 
        then Branch(idx, sep, this, that)
        else Branch(idx, sep, that, this)

    override protected[util] def forDominatingImpl(ctx: TraverseContext[D], mask: Int): Unit =
      if mask == 0 || ctx.isDominatedBy(point) then
        ctx.update(data0)
        if dataMore != null then
          dataMore.foreach(ctx.update)

    override def remove(point: Array[Double], data: D): KDTree[D] =
      if data0 == data then
        if dataMore == null then empty else
          val lastIndex = dataMore.size - 1
          data0 = dataMore(lastIndex)
          if lastIndex == 0 
          then dataMore = null
          else dataMore.remove(lastIndex)
          this
      else
        val idx = dataMore.indexOf(data)
        dataMore.remove(idx)
        if dataMore.isEmpty then dataMore = null
        this

    override def isEmpty: Boolean = false
  end Leaf
