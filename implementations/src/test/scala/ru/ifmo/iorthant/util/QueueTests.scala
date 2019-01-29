package ru.ifmo.iorthant.util

import java.util.Random

import org.junit.{Assert, Test}

class QueueTests {
  import QueueTests._

  @Test
  def smokeTest(): Unit = {
    val rng = new Random(834635623526L)
    val queue1 = new java.util.PriorityQueue[IntPair](1000)
    val queue1values = new Array[Int](1000)
    val queue1versions = new Array[Int](1000)
    val queue2 = new PriorityQueueWithReferences[PackedInt](1000)
    val queue2values = new Array[PackedInt](1000)
    val queue1live = new LiveDeadSet(1000)

    for (x <- 0 until 10000) {
      Assert.assertEquals(queue1live.nLive, queue2.size)
      if (queue1live.nLive < 1000) {
        val value = rng.nextInt()
        val index = queue1live.reviveRandom(rng)
        queue1values(index) = value
        queue1versions(index) += 1
        queue2values(index) = new PackedInt(value)
        queue1.add(new IntPair(value, index, queue1versions(index)))
        queue2.add(queue2values(index))
      } else if (queue1live.nLive > 0) {
        if (rng.nextBoolean()) {
          val index = queue1live.getRandomLive(rng)
          val oldValue = queue1values(index)
          val newValue = rng.nextInt()
          queue1values(index) = newValue
          queue1versions(index) += 1
          queue1.add(new IntPair(newValue, index, queue1versions(index)))
          val value2 = queue2values(index)
          value2.value = newValue
          if (newValue < oldValue) {
            queue2.updateAfterDecrease(value2)
          } else if (newValue > oldValue) {
            queue2.updateAfterIncrease(value2)
          }
        } else {
          var top: IntPair = queue1.poll()
          while (queue1versions(top.index) != top.version) {
            top = queue1.poll()
          }
          val index = top.index
          val top2 = queue2.removeSmallest()
          Assert.assertEquals("iteration " + x, top.value, top2.value)
          Assert.assertEquals(queue2values(top.index), top2) // not necessarily holds, but the tester will get destroyed otherwise

          val newValue = rng.nextInt()
          queue1values(index) = newValue
          queue1versions(index) += 1
          queue2values(index) = new PackedInt(newValue)
          queue1.add(new IntPair(newValue, index, queue1versions(index)))
          queue2.add(queue2values(index))
        }
      }
    }
  }
}

object QueueTests {
  class PackedInt(var value: Int) extends PriorityQueueWithReferences.HasIndex with Ordered[PackedInt] {
    override def compare(that: PackedInt): Int = Integer.compare(value, that.value)
  }
  class IntPair(val value: Int, val index: Int, val version: Int) extends Comparable[IntPair] {
    override def compareTo(o: IntPair): Int = Integer.compare(value, o.value)
  }
}
