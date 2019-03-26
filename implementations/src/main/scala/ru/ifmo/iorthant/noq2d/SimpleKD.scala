package ru.ifmo.iorthant.noq2d

import ru.ifmo.iorthant.util._
import ru.ifmo.iorthant.util.kd.{DecreasingTree, IncreasingTree}

class SimpleKD[@specialized(Specialization.defaultSet) T](minNonStrictCoordinate: Int)(implicit m: Monoid[T])
  extends NoUpdateIncrementalOrthantSearch[T] {

  override type DataPointHandle = DataWrapper[T]
  override type QueryPointHandle = QueryWrapper[T]

  override def newDataPointHandleArray(n: Int): Array[DataPointHandle] = new Array[DataPointHandle](n)
  override def newQueryPointHandleArray(n: Int): Array[QueryPointHandle] = new Array[QueryPointHandle](n)

  private var dataPoints: KDTree[DataPointHandle] = IncreasingTree.empty
  private var queryPoints: KDTree[QueryPointHandle] = DecreasingTree.empty

  override def addDataPoint(point: Array[Double], value: T): DataPointHandle = {
    val handle = new DataWrapper[T](point, value)
    dataPoints = dataPoints.add(point, handle)
    queryPoints.forDominating(new SimpleKD.AddContext[T](point, value, minNonStrictCoordinate))
    handle
  }

  override def addQueryPoint[@specialized(Specialization.defaultSet) I](point: Array[Double],
                                tracker: NoUpdateIncrementalOrthantSearch.UpdateTracker[T, I],
                                identifier: I): QueryPointHandle = {
    val value = makeQuery(point)
    val handle = new QueryWrapper.Tracking(point, value, tracker, identifier)
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
    queryPoints.forDominating(new SimpleKD.RemoveContext[T](handle.point, handle.value, minNonStrictCoordinate))
  }

  override def removeQueryPoint(handle: QueryPointHandle): Unit = {
    queryPoints = queryPoints.remove(handle.point, handle)
  }
}

object SimpleKD {
  private class QueryContext[@specialized(Specialization.defaultSet) T](val point: Array[Double],
                                                                        minNonStrictCoordinate: Int)
                                                                       (implicit m: Monoid[T])
    extends KDTree.TraverseContext[DataWrapper[T]] {
    private var v = m.zero
    def value: T = v
    override def update(data: DataWrapper[T]): Unit = {
      v = m.plus(v, data.value)
    }
    override def isDominatedBy(point: Array[Double]): Boolean = {
      Dominance.partial(point, this.point, minNonStrictCoordinate)
    }
  }

  private class AddContext[@specialized(Specialization.defaultSet) T](val point: Array[Double], val value: T,
                                                                      minNonStrictCoordinate: Int)
                                                                     (implicit m: Monoid[T])
    extends KDTree.TraverseContext[QueryWrapper[T]] {
    override def update(data: QueryWrapper[T]): Unit = {
      data.plus(value)
    }
    override def isDominatedBy(point: Array[Double]): Boolean = {
      Dominance.partial(this.point, point, minNonStrictCoordinate)
    }
  }

  private class RemoveContext[@specialized(Specialization.defaultSet) T](val point: Array[Double], val value: T,
                                                                         minNonStrictCoordinate: Int)
                                                                        (implicit m: HasNegation[T])
    extends KDTree.TraverseContext[QueryWrapper[T]] {
    override def update(data: QueryWrapper[T]): Unit = {
      data.minus(value)
    }
    override def isDominatedBy(point: Array[Double]): Boolean = {
      Dominance.partial(this.point, point, minNonStrictCoordinate)
    }
  }
}
