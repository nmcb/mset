package fpa
package data

import scala.annotation.tailrec

case class MultiSet(elements: IndexedSeq[MultiSet]):

  import MultiSet.*

  def +(that: MultiSet): MultiSet =
    MultiSet(this.elements ++ that.elements)

  def *(that: MultiSet): MultiSet =
    MultiSet(for { a <- this.elements ; b <- that.elements } yield a + b)

  def isZero: Boolean =
    this == Zero

  def isNatural: Boolean =
    elements.forall(_.isZero)

  def isPoly: Boolean =
    elements.forall(_.isNatural)

  def isMulti: Boolean =
    elements.forall(_.isPoly)

  def toInt: Int =
    if isNatural then elements.size else sys.error("not a natural number")

  def asNatural: String =
    toInt.toString

  def asPolynomial: String =
    assert(isPoly, "not a poly number")

    val products: Map[Int, Int] =
      elements.groupBy(identity).map(_.toInt -> _.size)

    def term(index: Int): Option[String] =
      products.get(index).map(product =>
        val variable = if index != 0 then "𝛼₀" else ""
        val exponent = if index >= 2 then index.toSuperScriptString else ""
        s"$product$variable$exponent"
      )

    List.tabulate(products.keys.max + 1)(term).flatten.mkString("+")

  def asMultiSet: String =
    elements.map(_.toString).mkString("[", " ", "]")

  override def toString: String =
    if      isNatural then asNatural
    else if isPoly    then asPolynomial
    else                   asMultiSet

type Nat   = MultiSet // in reality a MultiSet[Zero]
type Poly  = MultiSet // in reality a MultiSet[Nat]
type Multi = MultiSet // in reality a MultiSet[Poly]

object MultiSet:

  import Script.*

  def empty: MultiSet =
    MultiSet(IndexedSeq.empty[MultiSet])

  def apply(elements: MultiSet*): MultiSet =
    MultiSet(elements.toIndexedSeq)

  def apply(i: Int): Nat =
    fromInt(i)

  val  Zero : MultiSet = empty
  val   One : MultiSet = MultiSet(Zero)
  val   Two : MultiSet = MultiSet(Zero, Zero)
  val Three : MultiSet = MultiSet(Zero, Zero, Zero)
  val  Four : MultiSet = MultiSet(Zero, Zero, Zero, Zero)
  val  Five : MultiSet = MultiSet(Zero, Zero, Zero, Zero, Zero)
  val   Six : MultiSet = MultiSet(Zero, Zero, Zero, Zero, Zero, Zero)
  val Seven : MultiSet = MultiSet(Zero, Zero, Zero, Zero, Zero, Zero, Zero)
  val Eight : MultiSet = MultiSet(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero)
  val  Nine : MultiSet = MultiSet(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero)
  val   Ten : MultiSet = MultiSet(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero)

  def poly(ints: Int*): Poly =
    MultiSet(ints.map(fromInt).toIndexedSeq)

  val `𝛼₀`  : Poly = poly(1)
  val `𝛼₁`  : Poly = MultiSet(`𝛼₀`)
  val `𝛼₂`  : Poly = MultiSet(`𝛼₀` * `𝛼₀`)
  val `𝛼₃`  : Poly = MultiSet(`𝛼₀` * `𝛼₀` * `𝛼₀`)
  val `𝛼₄`  : Poly = MultiSet(`𝛼₀` * `𝛼₀` * `𝛼₀`)
  val `𝛼₅`  : Poly = MultiSet(`𝛼₀` * `𝛼₀` * `𝛼₀` * `𝛼₀`)
  val `𝛼₆`  : Poly = MultiSet(`𝛼₀` * `𝛼₀` * `𝛼₀` * `𝛼₀` * `𝛼₀`)
  val `𝛼₇`  : Poly = MultiSet(`𝛼₀` * `𝛼₀` * `𝛼₀` * `𝛼₀` * `𝛼₀` * `𝛼₀`)
  val `𝛼₈`  : Poly = MultiSet(`𝛼₀` * `𝛼₀` * `𝛼₀` * `𝛼₀` * `𝛼₀` * `𝛼₀` * `𝛼₀`)
  val `𝛼₉`  : Poly = MultiSet(`𝛼₀` * `𝛼₀` * `𝛼₀` * `𝛼₀` * `𝛼₀` * `𝛼₀` * `𝛼₀` * `𝛼₀`)
  val `𝛼₁₀` : Poly = MultiSet(`𝛼₀` * `𝛼₀` * `𝛼₀` * `𝛼₀` * `𝛼₀` * `𝛼₀` * `𝛼₀` * `𝛼₀` * `𝛼₀`)

  def fromInt(i: Int): Nat =
    assert(i >= 0, "must be a natural number")
    MultiSet(IndexedSeq.fill(i)(Zero))

  private def toScript(i: Int, script: Map[Char, Char]): String =
    @tailrec
    def loop(todo: List[Char], acc: String = ""): String =
      if todo.isEmpty then
        acc
      else
        loop(todo.tail, acc + script.getOrElse(todo.head, sys.error("not a digit")))
    loop(i.toString.toList)

  extension (i: Int) def toSubScriptString: String =
    toScript(i, subscript)

  extension (i: Int) def toSuperScriptString: String =
    toScript(i, superscript)