package ru.ifmo.iorthant.noq2d

import ru.ifmo.iorthant.util._

class SimpleKDBounded[@specialized(Specialization.defaultSet) T](minNonStrictCoordinate: Int)(implicit m: Monoid[T])
  extends NoUpdateIncrementalOrthantSearch[T] {

  override type DataPointHandle = PlainArray.DataWrapper[T]
  override type QueryPointHandle = PlainArray.QueryWrapper[T]

  override def newDataPointHandleArray(n: Int): Array[DataPointHandle] = new Array[DataPointHandle](n)
  override def newQueryPointHandleArray(n: Int): Array[QueryPointHandle] = new Array[QueryPointHandle](n)

  private var dataPoints: KDTree[DataPointHandle] = KDTree.empty
  private var queryPoints: KDTree[QueryPointHandle] = KDTree.empty
  private var usableCoordsArray: Array[Boolean] = _

  private def getUsableCoordinates(point: Array[Double]): Array[Boolean] = {
    if (usableCoordsArray == null || usableCoordsArray.length < point.length) {
      usableCoordsArray = new Array[Boolean](point.length)
      java.util.Arrays.fill(usableCoordsArray, true)
    }
    usableCoordsArray
  }

  override def addDataPoint(point: Array[Double], value: T): DataPointHandle = {
    val handle = new PlainArray.DataWrapper[T](point, value)
    dataPoints = dataPoints.add(point, handle)
    queryPoints.forDominatingBounded(
      new SimpleKD.AddContext[T](Arrays.negate(point), value, minNonStrictCoordinate),
      getUsableCoordinates(point))
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
    dataPoints.forDominatingBounded(context, getUsableCoordinates(point))
    context.value
  }

  override def removeDataPoint(handle: DataPointHandle)
                              (implicit hm: HasMinus[T]): Unit = {
    dataPoints = dataPoints.remove(handle.point, handle)
    queryPoints.forDominatingBounded(
      new SimpleKD.RemoveContext[T](Arrays.negate(handle.point), handle.value, minNonStrictCoordinate),
      getUsableCoordinates(handle.point))
  }

  override def removeQueryPoint(handle: QueryPointHandle): Unit = {
    queryPoints = queryPoints.remove(handle.point, handle)
  }
}


