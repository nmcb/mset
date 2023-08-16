package fpa
package data
package test

import org.scalatest.*
import funsuite.*

class MultiSetTest extends AnyFunSuite:

  import MultiSet.*

  test("empty constructor") {
    assertResult(MultiSet(IndexedSeq.empty))(actual = empty)
  }

  test("Zero constructor") {
    assertResult(expected = empty)(actual = Zero)
    assert(Zero.isZero)
    assert(Zero.isNat)
    assert(Zero.isPoly)
  }

  test( "natural number constructors") {
    val nat0 = Zero
    assert(nat0.isNat)
    assert(nat0.isPoly)

    val nat1 = MultiSet(Zero)
    assert(nat1.isNat)
    assert(nat1.isPoly)

    val nat2 = MultiSet(Zero, Zero)
    assert(nat2.isNat)
    assert(nat2.isPoly)

    val nat3 = MultiSet(Zero, Zero, Zero)
    assert(nat3.isNat)
    assert(nat3.isPoly)

    assertResult(expected = nat0)(actual = fromInt(0))
    assertResult(expected = nat1)(actual = fromInt(1))
    assertResult(expected = nat2)(actual = fromInt(2))
    assertResult(expected = nat3)(actual = fromInt(3))
  }

  test("natural numbers are closed under addition") {
    assertResult(expected = fromInt(3))(actual = fromInt(3) + fromInt(0))
    assertResult(expected = fromInt(3))(actual = fromInt(2) + fromInt(1))
    assertResult(expected = fromInt(3))(actual = fromInt(1) + fromInt(2))
    assertResult(expected = fromInt(3))(actual = fromInt(0) + fromInt(3))
    assert((fromInt(3) + fromInt(0)).isNat)
    assert((fromInt(2) + fromInt(1)).isNat)
    assert((fromInt(1) + fromInt(2)).isNat)
    assert((fromInt(0) + fromInt(3)).isNat)
  }

  test("natural number under addition is associative") {
    assertResult(expected = fromInt(3) + (fromInt(0) + fromInt(3)))(actual = (fromInt(3) + fromInt(0)) + fromInt(3))
    assertResult(expected = fromInt(2) + (fromInt(1) + fromInt(3)))(actual = (fromInt(2) + fromInt(1)) + fromInt(3))
    assertResult(expected = fromInt(1) + (fromInt(2) + fromInt(3)))(actual = (fromInt(1) + fromInt(2)) + fromInt(3))
    assertResult(expected = fromInt(0) + (fromInt(3) + fromInt(3)))(actual = (fromInt(0) + fromInt(3)) + fromInt(3))
  }

  test("poly number constructors") {
    val poly0 = poly(1, 1, 2)
    assert(poly0.isPoly)

    val poly1 = poly(13)
    assert(poly1.isPoly)

    val poly2 = poly(0, 0)
    assert(poly2.isPoly)

    val poly3 = poly()
    assert(poly3.isPoly)

    assertResult(expected = poly0)(actual = MultiSet(fromInt(1), fromInt(1), fromInt(2)))
    assertResult(expected = poly1)(actual = MultiSet(fromInt(13)))
    assertResult(expected = poly2)(actual = MultiSet(fromInt(0), fromInt(0)))
    assertResult(expected = poly3)(actual = MultiSet.empty)
  }

  test("poly numbers are closed under addition") {
    val addition = poly(3, 3, 4) + empty + poly(0) + poly(3, 7)
    val result   = poly(3, 3, 4, 0, 3, 7)
    assertResult(expected = result)(actual = addition)
    assert(addition.isPoly)
  }

  test("multi number constructor") {
    val multi0 = MultiSet(poly(1, 1), poly(1, 1))
    assert(multi0.isMulti)

    val multi1 = MultiSet(poly(3, 5, 11))
    assert(multi1.isMulti)

    val multi2 = MultiSet(Zero)
    assert(multi2.isMulti)

    val multi3 = MultiSet.empty
    assert(multi3.isMulti)

    assertResult(expected = multi0)(actual = MultiSet(MultiSet(fromInt(1), fromInt(1)), MultiSet(fromInt(1), fromInt(1))))
    assertResult(expected = multi1)(actual = MultiSet(MultiSet(fromInt(3), fromInt(5), fromInt(11))))
    assertResult(expected = multi2)(actual = MultiSet(MultiSet()))
    assertResult(expected = multi3)(actual = MultiSet())
  }

  test("multi numbers are closed under addition") {
    val addition = MultiSet(poly(2), poly(1, 3, 3)) + MultiSet(poly(0), poly(0), poly(5, 7))
    val result   = MultiSet(poly(2), poly(1, 3, 3), poly(0), poly(0), poly(5, 7))
    assertResult(expected = result)(actual = addition)
    assert(addition.isMulti)
  }

  test("multi number multiplication on natural numbers") {
    assertResult(expected = Zero)(actual = Zero * Zero)
    assertResult(expected = fromInt(6))(actual = fromInt(2) * fromInt(3))
  }

  test("multi number multiplication on poly numbers") {
    assertResult(expected = poly(3, 3, 2, 4, 4, 3))(actual = poly(2, 3) * poly(1, 1, 0))
    assertResult(expected = poly(3, 8, 6, 11, 6, 11, 9, 14))(actual = poly(0, 3) * poly(1, 4) * poly(2, 7))
  }

  test("multi number multiplication on multi numbers") {
    val multiplication = MultiSet(poly(0, 0, 2) , poly(3, 8)) * MultiSet(poly(1, 1), fromInt(2), poly(9))
    val result         = MultiSet(poly(0, 0, 2, 1, 1), poly(0, 0, 2, 0, 0), poly(0, 0, 2, 9), poly(3, 8, 1, 1), poly(3, 8, 0, 0), poly(3, 8, 9))
    assertResult(expected = result)(actual = multiplication)
  }

  test("poly number represented as polynomial") {
    val number = poly(0, 0, 1, 0, 3, 4)
    assertResult("3+1𝛼₀+1𝛼₀³+1𝛼₀⁴")(number.asPolynomial)
  }

  /** @see https://youtu.be/CScJqApRPZg?t=474 */
  test("addition on pure msets") {

    val addition =
      val a = MultiSet(poly(3), poly(3), poly(12))
      val b = MultiSet(Zero, Zero)
      val c = MultiSet(poly(1), poly(3), Four)
      a + b + c

    val result =
      MultiSet(
        poly(3),
        poly(3),
        poly(12),
        Zero, Zero,
        poly(1),
        poly(3),
        Four
    )
    assertResult(expected = result)(actual = addition)
  }

  /** @see https://youtu.be/CScJqApRPZg?t=598 */
  test("multiplication on pure msets") {
    val SixTeen =
      MultiSet(
        Zero, Zero, Zero, Zero,
        Zero, Zero, Zero, Zero,
        Zero, Zero, Zero, Zero,
        Zero, Zero, Zero, Zero
      )

    val multiplication =
      val a = MultiSet(poly(2), Three)
      val b = MultiSet(poly(4), poly(4))
      val c = MultiSet(poly(16), Zero)
      a * b * c

    val result =
      MultiSet(
        poly(2, 4, 16),
        poly(2, 4),
        poly(2, 4, 16),
        poly(2, 4),
        MultiSet(Zero, Zero, Zero, Four, SixTeen),
        MultiSet(Zero, Zero, Zero, Four),
        MultiSet(Zero, Zero, Zero, Four, SixTeen),
        MultiSet(Zero, Zero, Zero, Four)
      )
    assertResult(expected = result)(actual = multiplication)
  }

  /** @see https://youtu.be/CScJqApRPZg?t=730 */
  test("addition of poly variables") {
    assertResult(expected = poly(1, 1))(actual = `𝛼₀` + `𝛼₀`)
    assertResult(expected = poly(1, 1, 1))(actual = `𝛼₀` + `𝛼₀` + `𝛼₀`)
    assertResult(expected = poly(1, 1, 1, 1))(actual = `𝛼₀` + `𝛼₀` + `𝛼₀` + `𝛼₀`)
  }

  /** @see https://youtu.be/CScJqApRPZg?t=740 */
  test("multiplication of poly variables") {
    assertResult(expected = poly(1, 1))(actual =   Two * `𝛼₀`)
    assertResult(expected = poly(1, 1, 1))(actual = Three * `𝛼₀`)
    assertResult(expected = poly(1, 1, 1, 1))(actual =  Four * `𝛼₀`)
  }

  /** @see https://youtu.be/CScJqApRPZg?t=740 */
  test("exponentiation of poly variables") {
    assertResult(expected = poly(2))(actual = `𝛼₀` * `𝛼₀`)
    assertResult(expected = poly(3))(actual = `𝛼₀` * `𝛼₀` * `𝛼₀`)
    assertResult(expected = poly(4))(actual = `𝛼₀` * `𝛼₀` * `𝛼₀` * `𝛼₀`)
  }

  /** @see https://youtu.be/CScJqApRPZg?t=740 */
  test("poly variable indexing") {
    assertResult(expected = MultiSet(MultiSet(Zero)))(actual = `𝛼₀`)
    assertResult(expected = MultiSet(MultiSet(MultiSet(Zero))))(actual = `𝛼₁`)
    assertResult(expected = MultiSet(MultiSet(MultiSet(MultiSet(Zero)))))(actual = `𝛼₂`)
  }

