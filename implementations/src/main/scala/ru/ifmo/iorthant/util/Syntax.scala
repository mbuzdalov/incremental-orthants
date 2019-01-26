package ru.ifmo.iorthant.util

import scala.collection.mutable.ArrayBuffer

object Syntax {
  implicit class any2addTo[T](val value: T) extends AnyVal {
    def addTo[U >: T](that: ArrayBuffer[U]): T = {
      that += value
      value
    }
  }
}
