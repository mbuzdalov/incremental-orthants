package ru.ifmo.iorthant.noq2d

import java.util.{ArrayDeque => JQueue}

import ru.ifmo.iorthant.util._

class SimpleKD[@specialized(Specialization.defaultSet) T](minNonStrictCoordinate: Int)(implicit m: Monoid[T])
  extends NoUpdateIncrementalOrthantSearch[T] {

  override type DataPointHandle = PlainArray.DataWrapper[T]
  override type QueryPointHandle = PlainArray.QueryWrapper[T]

  override def newDataPointHandleArray(n: Int): Array[DataPointHandle] = new Array[DataPointHandle](n)
  override def newQueryPointHandleArray(n: Int): Array[QueryPointHandle] = new Array[QueryPointHandle](n)

  private var dataPoints: KDTree[DataPointHandle] = KDTree.empty
  private var queryPoints: KDTree[QueryPointHandle] = KDTree.empty
  private val dataPointQueue: JQueue[KDTree[DataPointHandle]] = new JQueue()
  private val queryPointQueue: JQueue[KDTree[QueryPointHandle]] = new JQueue()

  override def addDataPoint(point: Array[Double], value: T): DataPointHandle = {
    val handle = new PlainArray.DataWrapper[T](point, value)
    dataPoints = dataPoints.add(point, handle)
    queryPoints.traverseDominating(
      new SimpleKD.AddContext[T](Arrays.negate(point), value, minNonStrictCoordinate),
      queryPointQueue)
    handle
  }

  override def addQueryPoint[I](point: Array[Double],
                                tracker: NoUpdateIncrementalOrthantSearch.UpdateTracker[T, I],
                                identifier: I): QueryPointHandle = {
    val value = makeQuery(point)
    val handle = new PlainArray.QueryWrapperImpl(Arrays.negate(point), value, tracker, identifier)
    queryPoints = queryPoints.add(handle.point, handle)
    handle
  }

  override def makeQuery(point: Array[Double]): T = {
    val context = new SimpleKD.QueryContext[T](point, minNonStrictCoordinate)
    dataPoints.traverseDominating(context, dataPointQueue)
    context.value
  }

  override def removeDataPoint(handle: DataPointHandle)
                              (implicit hm: HasMinus[T]): Unit = {
    dataPoints = dataPoints.remove(handle.point, handle)
    queryPoints.traverseDominating(
      new SimpleKD.RemoveContext[T](Arrays.negate(handle.point), handle.value, minNonStrictCoordinate),
      queryPointQueue)
  }

  override def removeQueryPoint(handle: QueryPointHandle): Unit = {
    queryPoints = queryPoints.remove(handle.point, handle)
  }
}

object SimpleKD {
  class QueryContext[@specialized(Specialization.defaultSet) T](val point: Array[Double], minNonStrictCoordinate: Int)
                                                               (implicit m: Monoid[T])
    extends KDTree.TraverseContext[PlainArray.DataWrapper[T]] {
    private var v = m.zero
    def value: T = v
    override def update(data: PlainArray.DataWrapper[T]): Unit = {
      v = m.plus(v, data.value)
    }
    override def dominates(lhs: Array[Double], rhs: Array[Double]): Boolean = {
      Dominance.partial(lhs, rhs, minNonStrictCoordinate)
    }
  }

  class AddContext[@specialized(Specialization.defaultSet) T](val point: Array[Double], val value: T, minNonStrictCoordinate: Int)
                                                             (implicit m: Monoid[T])
    extends KDTree.TraverseContext[PlainArray.QueryWrapper[T]] {
    override def update(data: PlainArray.QueryWrapper[T]): Unit = {
      data.plus(value)
    }
    override def dominates(lhs: Array[Double], rhs: Array[Double]): Boolean = {
      Dominance.partial(lhs, rhs, minNonStrictCoordinate)
    }
  }

  class RemoveContext[@specialized(Specialization.defaultSet) T](val point: Array[Double], val value: T, minNonStrictCoordinate: Int)
                                                                (implicit m: HasMinus[T])
    extends KDTree.TraverseContext[PlainArray.QueryWrapper[T]] {
    override def update(data: PlainArray.QueryWrapper[T]): Unit = {
      data.minus(value)
    }
    override def dominates(lhs: Array[Double], rhs: Array[Double]): Boolean = {
      Dominance.partial(lhs, rhs, minNonStrictCoordinate)
    }
  }
}
