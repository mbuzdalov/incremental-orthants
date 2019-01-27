package ru.ifmo.iorthant.util

import java.util.Random

class LiveDeadSet(n: Int) {
  private[this] var firstDead = 0
  private[this] val elements = Array.tabulate(n)(identity)

  def killRandom(rng: Random): Int = {
    require(firstDead > 0, "no more live elements")
    val index = rng.nextInt(firstDead)
    val rv = elements(index)
    firstDead -= 1
    elements(index) = elements(firstDead)
    elements(firstDead) = rv
    rv
  }

  def reviveRandom(rng: Random): Int = {
    require(firstDead < n, "no more dead elements")
    val index = firstDead + rng.nextInt(n - firstDead)
    val rv = elements(index)
    elements(index) = elements(firstDead)
    elements(firstDead) = rv
    firstDead += 1
    rv
  }

  def nLive: Int = firstDead
  def nDead: Int = n - firstDead
}
