package ru.ifmo.iorthant.collection

import org.junit.{Assert, Test}

class AddOneGetOneTests {
  private implicit val intMonoid: Monoid[Int] = TestMonoid
  private def performHandChecks(ds: AddOne[Int] with GetOne[Int]): Unit = {
    val h0 = ds.add(Array(1, 2, 3, 4), 5)
    val h1 = ds.add(Array(2, 3, 4, 5), 6)
    val h2 = ds.add(Array(0, 0, 0, 0), 7)
    val h3 = ds.add(Array(0, 0, 0, 0), 8)
    Assert.assertEquals(5, ds.get(h0))
    Assert.assertEquals(6, ds.get(h1))
    Assert.assertEquals(7, ds.get(h2))
    Assert.assertEquals(8, ds.get(h3))
  }

  @Test def plainArray(): Unit = performHandChecks(new PlainArray[Int]())
}
