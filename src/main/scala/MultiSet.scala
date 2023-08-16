package fpa
package data

case class MultiSet(elements: IndexedSeq[MultiSet]):

  import MultiSet.*

  def +(that: MultiSet): MultiSet =
    MultiSet(this.elements ++ that.elements)

  def *(that: MultiSet): MultiSet =
    MultiSet(for { a <- this.elements ; b <- that.elements } yield a + b)

  def isZero: Boolean =
    this == Zero

  def isNat: Boolean =
    elements.forall(_.isZero)

  def isPoly: Boolean =
    elements.forall(_.isNat)

  def isMulti: Boolean =
    elements.forall(_.isPoly)

  def toInt: Int =
    if isNat then elements.size else sys.error("not a natural number")

  def asNatural: String =
    toInt.toString

  def asPolynomial: String =
    assert(isPoly, "not a poly number")

    val products: Map[Int, Int] =
      elements.groupBy(identity).map((p,ps) => p.toInt -> ps.size)

    def term(i: Int): Option[String] =
      products.get(i).map(p =>
        val v = if i != 0 then s"𝛼${toSubScriptString(0)}" else ""
        val e = if i >= 2 then toSuperScriptString(i) else ""
        s"$p$v$e"
      )

    List.tabulate(products.keys.max + 1)(term).flatten.mkString("+")

  def asMultiSet: String =
    elements.map(_.toString).mkString("[", " ", "]")

  override def toString: String =
    if      isNat  then asNatural
    else if isPoly then asPolynomial
    else                asMultiSet

type Nat   = MultiSet // in reality a MultiSet[Zero]
type Poly  = MultiSet // in reality a MultiSet[Nat]
type Multi = MultiSet // in reality a MultiSet[Poly]

object MultiSet extends App:

  def empty: MultiSet =
    MultiSet(IndexedSeq.empty[MultiSet])

  def apply(elements: MultiSet*): MultiSet =
    MultiSet(elements.toIndexedSeq)

  def apply(i: Int): Nat=
    fromInt(i)

  val Zero: MultiSet = empty
  val One: MultiSet = MultiSet(Zero)
  val Two: MultiSet = MultiSet(Zero, Zero)
  val Three: MultiSet = MultiSet(Zero, Zero, Zero)
  val Four: MultiSet = MultiSet(Zero, Zero, Zero, Zero)
  val Five: MultiSet = MultiSet(Zero, Zero, Zero, Zero, Zero)
  val Six: MultiSet = MultiSet(Zero, Zero, Zero, Zero, Zero, Zero)
  val Seven: MultiSet = MultiSet(Zero, Zero, Zero, Zero, Zero, Zero, Zero)
  val Eight: MultiSet = MultiSet(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero)
  val Nine: MultiSet = MultiSet(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero)
  val Ten: MultiSet = MultiSet(Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero)

  def poly(naturals: Int*): Poly =
    MultiSet(naturals.map(fromInt).toIndexedSeq)

  val `𝛼₀` : Poly = poly(1)

  def fromInt(i: Int): Nat =
    assert(i >= 0, "must be a natural number")
    MultiSet(IndexedSeq.fill(i)(Zero))

  extension (i: Int) def toSubScriptString: String =
    def loop(todo: List[Char], acc: String = ""): String =
      todo match
        case '0' :: rest => loop(rest, acc + '\u2080')
        case '1' :: rest => loop(rest, acc + '\u2081')
        case '2' :: rest => loop(rest, acc + '\u2082')
        case '3' :: rest => loop(rest, acc + '\u2083')
        case '4' :: rest => loop(rest, acc + '\u2084')
        case '5' :: rest => loop(rest, acc + '\u2085')
        case '6' :: rest => loop(rest, acc + '\u2086')
        case '7' :: rest => loop(rest, acc + '\u2087')
        case '8' :: rest => loop(rest, acc + '\u2088')
        case '9' :: rest => loop(rest, acc + '\u2089')
        case '-' :: rest => loop(rest, acc + '\u208B')
        case         Nil => acc
        case           _ => sys.error("not a decimal")
    loop(i.toString.toList)

  extension (i: Int) def toSuperScriptString: String =
    def loop(todo: List[Char], acc: String = ""): String =
      todo match
        case '0' :: rest => loop(rest, acc + '\u2070')
        case '1' :: rest => loop(rest, acc + '\u00B9')
        case '2' :: rest => loop(rest, acc + '\u00B2')
        case '3' :: rest => loop(rest, acc + '\u00B3')
        case '4' :: rest => loop(rest, acc + '\u2074')
        case '5' :: rest => loop(rest, acc + '\u2075')
        case '6' :: rest => loop(rest, acc + '\u2076')
        case '7' :: rest => loop(rest, acc + '\u2077')
        case '8' :: rest => loop(rest, acc + '\u2078')
        case '9' :: rest => loop(rest, acc + '\u2079')
        case '-' :: rest => loop(rest, acc + '\u208B')
        case Nil => acc
        case _ => sys.error("not a decimal")

    loop(i.toString.toList)


  // playground

  val addZero = MultiSet(fromInt(2), fromInt(1), fromInt(0))
  println(addZero)
