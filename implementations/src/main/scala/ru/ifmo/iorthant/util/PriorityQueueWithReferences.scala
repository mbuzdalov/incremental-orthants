package ru.ifmo.iorthant.util

class PriorityQueueWithReferences[T <: PriorityQueueWithReferences.HasIndex with Comparable[T]](maxSize: Int) {

}

object PriorityQueueWithReferences {
  trait HasIndex {
    private[PriorityQueueWithReferences] var index: Int = _
  }
}
