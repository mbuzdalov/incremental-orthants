package ru.ifmo.iorthant.util

object Bits {
  def intMask(nBits: Int): Int = {
    if (nBits > 32) {
      throw new IllegalArgumentException(s"Cannot make up a $nBits-bit mask in a 32-bit integer")
    }
    if (nBits == 32) -1 else (1 << nBits) - 1
  }
}
