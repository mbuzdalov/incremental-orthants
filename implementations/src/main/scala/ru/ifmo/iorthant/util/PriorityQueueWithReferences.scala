package ru.ifmo.iorthant.util

import scala.annotation.tailrec
import scala.reflect.ClassTag

class PriorityQueueWithReferences[T <: PriorityQueueWithReferences.HasIndex with Comparable[T]](maxSize: Int)
                                                                                               (implicit tag: ClassTag[T]) {
  private[this] val arr = new Array[T](maxSize)
  private[this] var cnt: Int = 0

  def size: Int = cnt

  def add(element: T): Unit = {
    arr(cnt) = element
    siftUp(cnt)
    cnt += 1
  }

  def foreach(fun: T => Unit): Unit = {
    var i = 0
    while (i < cnt) {
      fun(arr(i))
      i += 1
    }
  }

  def foreachWithIndex(fun: (T, Int) => Unit): Unit = {
    var i = 0
    while (i < cnt) {
      fun(arr(i), i)
      i += 1
    }
  }

  def removeSmallest(): T = {
    val rv = arr(0)
    cnt -= 1
    if (cnt > 0) {
      arr(0) = arr(cnt)
      siftDown(0)
    }
    arr(cnt) = _
    rv.index = -1
    rv
  }

  def updateAfterIncrease(element: T): Unit = if (element.index >= 0) siftDown(element.index)
  def updateAfterDecrease(element: T): Unit = if (element.index >= 0) siftUp(element.index)

  @tailrec
  private def siftUp(i: Int): Unit = {
    val ai = arr(i)
    if (i > 0) {
      val p = (i - 1) >>> 1
      val ap = arr(p)
      if (ap.compareTo(ai) > 0) {
        arr(i) = ap
        ap.index = i
        arr(p) = ai
        siftUp(p)
      } else {
        ai.index = i
      }
    } else {
      ai.index = i
    }
  }

  @tailrec
  private def siftDown(i: Int): Unit = {
    val l = (i << 1) + 1
    val ai = arr(i)
    if (l < cnt) {
      val al = arr(l)
      var b = l
      var ab = al
      val r = l + 1
      if (r < cnt) {
        val ar = arr(r)
        if (al.compareTo(ar) > 0) {
          b = r
          ab = ar
        }
      }
      if (ai.compareTo(ab) > 0) {
        arr(i) = ab
        ab.index = i
        arr(b) = ai
        siftDown(b)
      } else {
        ai.index = i
      }
    } else {
      ai.index = i
    }
  }
}

object PriorityQueueWithReferences {
  trait HasIndex {
    private[PriorityQueueWithReferences] var index: Int = _
  }
}
