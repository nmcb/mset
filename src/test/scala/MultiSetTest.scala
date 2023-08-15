package fpa
package data
package test

import org.scalatest.*
import funsuite.*

class MultiSetTest extends AnyFunSuite:

  test("empty") {
    assertResult(MultiSet(IndexedSeq.empty))(actual = MultiSet.empty)
  }
