package ru.ifmo.iorthant.noq2d

class PlainArrayTests extends Tests {
  override def makeDataStructure(): NoUpdateIncrementalOrthantSearch[Int] = new PlainArray[Int]()
}
