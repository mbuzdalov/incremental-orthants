package ru.ifmo.iorthant.util

import java.util.Random

class LiveDeadSet(n: Int) {
  private[this] var firstDead = 0
  private[this] val elements = Array.tabulate(n)(identity)

  private[this] def getRandomLiveIndex(rng: Random): Int = {
    require(firstDead > 0, "no more live elements")
    rng.nextInt(firstDead)
  }

  private[this] def getRandomDeadIndex(rng: Random): Int = {
    require(firstDead < n, "no more dead elements")
    firstDead + rng.nextInt(n - firstDead)
  }

  def getRandomLive(rng: Random): Int = elements(getRandomLiveIndex(rng))
  def getRandomDead(rng: Random): Int = elements(getRandomDeadIndex(rng))

  def killRandom(rng: Random): Int = {
    val index = getRandomLiveIndex(rng)
    val rv = elements(index)
    firstDead -= 1
    elements(index) = elements(firstDead)
    elements(firstDead) = rv
    rv
  }

  def reviveRandom(rng: Random): Int = {
    val index = getRandomDeadIndex(rng)
    val rv = elements(index)
    elements(index) = elements(firstDead)
    elements(firstDead) = rv
    firstDead += 1
    rv
  }

  def nLive: Int = firstDead
  def nDead: Int = n - firstDead
}
