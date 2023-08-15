package fpa
package data

case class MultiSet(elements: IndexedSeq[MultiSet])

object MultiSet:

  def empty: MultiSet = MultiSet(IndexedSeq.empty[Nothing])
