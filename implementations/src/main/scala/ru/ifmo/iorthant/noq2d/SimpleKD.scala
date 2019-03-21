package ru.ifmo.iorthant.noq2d

import ru.ifmo.iorthant.util._

class SimpleKD[@specialized(Specialization.defaultSet) T](minNonStrictCoordinate: Int)(implicit m: Monoid[T])
  extends NoUpdateIncrementalOrthantSearch[T] {

  override type DataPointHandle = DataWrapper[T]
  override type QueryPointHandle = QueryWrapper[T]

  override def newDataPointHandleArray(n: Int): Array[DataPointHandle] = new Array[DataPointHandle](n)
  override def newQueryPointHandleArray(n: Int): Array[QueryPointHandle] = new Array[QueryPointHandle](n)

  private var dataPoints: KDTree[DataPointHandle] = KDTree.empty
  private var queryPoints: KDTree[QueryPointHandle] = KDTree.empty

  override def addDataPoint(point: Array[Double], value: T): DataPointHandle = {
    val handle = new DataWrapper[T](point, value)
    dataPoints = dataPoints.add(point, handle)
    queryPoints.forDominating(new SimpleKD.AddContext[T](Arrays.negate(point), value, minNonStrictCoordinate))
    handle
  }

  override def addQueryPoint[@specialized(Specialization.defaultSet) I](point: Array[Double],
                                tracker: NoUpdateIncrementalOrthantSearch.UpdateTracker[T, I],
                                identifier: I): QueryPointHandle = {
    val value = makeQuery(point)
    val handle = new QueryWrapper.Tracking(Arrays.negate(point), value, tracker, identifier)
    queryPoints = queryPoints.add(handle.point, handle)
    handle
  }

  override def makeQuery(point: Array[Double]): T = {
    val context = new SimpleKD.QueryContext[T](point, minNonStrictCoordinate)
    dataPoints.forDominating(context)
    context.value
  }

  override def removeDataPoint(handle: DataPointHandle)
                              (implicit hm: HasNegation[T]): Unit = {
    dataPoints = dataPoints.remove(handle.point, handle)
    queryPoints.forDominating(new SimpleKD.RemoveContext[T](Arrays.negate(handle.point), handle.value, minNonStrictCoordinate))
  }

  override def removeQueryPoint(handle: QueryPointHandle): Unit = {
    queryPoints = queryPoints.remove(handle.point, handle)
  }
}

object SimpleKD {
  class QueryContext[@specialized(Specialization.defaultSet) T](val point: Array[Double], minNonStrictCoordinate: Int)
                                                               (implicit m: Monoid[T])
    extends KDTree.TraverseContext[DataWrapper[T]] {
    private var v = m.zero
    def value: T = v
    override def update(data: DataWrapper[T]): Unit = {
      v = m.plus(v, data.value)
    }
    override def dominates(lhs: Array[Double], rhs: Array[Double]): Boolean = {
      Dominance.partial(lhs, rhs, minNonStrictCoordinate)
    }
  }

  class AddContext[@specialized(Specialization.defaultSet) T](val point: Array[Double], val value: T, minNonStrictCoordinate: Int)
                                                             (implicit m: Monoid[T])
    extends KDTree.TraverseContext[QueryWrapper[T]] {
    override def update(data: QueryWrapper[T]): Unit = {
      data.plus(value)
    }
    override def dominates(lhs: Array[Double], rhs: Array[Double]): Boolean = {
      Dominance.partial(lhs, rhs, minNonStrictCoordinate)
    }
  }

  class RemoveContext[@specialized(Specialization.defaultSet) T](val point: Array[Double], val value: T, minNonStrictCoordinate: Int)
                                                                (implicit m: HasNegation[T])
    extends KDTree.TraverseContext[QueryWrapper[T]] {
    override def update(data: QueryWrapper[T]): Unit = {
      data.minus(value)
    }
    override def dominates(lhs: Array[Double], rhs: Array[Double]): Boolean = {
      Dominance.partial(lhs, rhs, minNonStrictCoordinate)
    }
  }
}
