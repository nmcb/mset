package fpa
package data


case class MultiSet(elements: IndexedSeq[MultiSet]):

  import MultiSet.*

  def +(that: MultiSet): MultiSet =
    MultiSet(this.elements ++ that.elements)

  def *(that: MultiSet): MultiSet =
    MultiSet(for { a <- this.elements ; b <- that.elements } yield a + b)

  def isZero: Boolean =
    this == zero
  def isNat: Boolean =
    elements.forall(_.isZero)

  def isPoly: Boolean =
    elements.forall(_.isNat)

  def isMulti: Boolean =
    elements.forall(_.isPoly)

  def asNat: Int =
    if isNat then elements.size else sys.error("not a natural number")

  override def toString: String =
    if isNat then asNat.toString else elements.map(_.toString).mkString("["," ","]")


object MultiSet:

  def apply(elements: MultiSet*): MultiSet =
    MultiSet(elements.toIndexedSeq)

  def empty: MultiSet =
    MultiSet(IndexedSeq.empty[Nothing])

  def zero: MultiSet =
    empty

  def poly(naturals: Int*): MultiSet =
    MultiSet(naturals.map(fromInt).toIndexedSeq)

  def fromInt(i: Int): MultiSet =
    assert(i >= 0, "must be a natural number")
    MultiSet(IndexedSeq.fill(i)(zero))
