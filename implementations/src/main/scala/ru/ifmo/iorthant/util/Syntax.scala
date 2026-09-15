package ru.ifmo.iorthant.util

import scala.collection.mutable.ArrayBuffer

object Syntax:
  extension [T] (value: T)
    def addTo[U >: T](that: ArrayBuffer[U]): T =
      that += value
      value
