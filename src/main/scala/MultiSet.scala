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

  def toInt: Int =
    if isNat then elements.size else sys.error("not a natural number")

  def asNatString: String =
    toInt.toString

  def asPolyString: String =
    assert(isPoly, "not a poly number")

    val products: Map[Int, Int] =
      elements.groupBy(identity).map((p,ps) => p.toInt -> ps.size)

    def term(i: Int): Option[String] =
      products.get(i).map(p =>
        val v = if i != 0 then s"𝛼" else ""
        val e = if i >= 2 then asSuperScriptString(i) else ""
        s"$p$v$e"
      )

    List.tabulate(products.keys.max + 1)(term).flatten.mkString(" + ")

  def asMultiSetString: String =
    elements.map(_.toString).mkString("[", " ", "]")

  override def toString: String =
    if      isNat  then asNatString
    else if isPoly then asPolyString
    else                asMultiSetString

type Nat  = MultiSet
type Poly = MultiSet // in reality a MultiSet[Nat]

object MultiSet extends App:

  def apply(elements: MultiSet*): MultiSet =
    MultiSet(elements.toIndexedSeq)

  def apply(i: Int): Nat=
    fromInt(i)

  def empty: MultiSet =
    MultiSet(IndexedSeq.empty[MultiSet])

  def zero: Nat =
    empty

  def poly(naturals: Int*): Poly =
    MultiSet(naturals.map(fromInt).toIndexedSeq)

  def fromInt(i: Int): Nat =
    assert(i >= 0, "must be a natural number")
    MultiSet(IndexedSeq.fill(i)(zero))

  extension (i: Int) def asSubScriptString: String =
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

  extension (i: Int) def asSuperScriptString: String =
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
