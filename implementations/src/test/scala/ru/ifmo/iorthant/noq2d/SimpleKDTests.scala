package ru.ifmo.iorthant.noq2d

class SimpleKDTests extends Tests {
  override def makeDataStructure(): NoUpdateIncrementalOrthantSearch[Int] = new SimpleKD[Int](0)
}
