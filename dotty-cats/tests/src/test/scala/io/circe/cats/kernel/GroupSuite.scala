package io.circe.cats.kernel

import io.circe.cats.kernel.Group
import io.circe.cats.kernel.laws.discipline.GroupTests
import given io.circe.cats.syntax.eq._
import io.circe.cats.tests.CatsSuite

object GroupSuite extends CatsSuite {
  test("combine minValue") {
    assert(Group[Int].combineN(1, Int.MinValue) === Int.MinValue)
  }

  test("combine negative") {
    assert(Group[Int].combineN(1, -1) === -1)
    assert(Group[Int].combineN(1, -10) === -10)
  }

  test("companion object syntax") {
    assert(Group[Int].inverse(1) === -1)
    assert(Group[Int].remove(1, 2) === -1)
  }

  checkAll("Int", GroupTests[Int].group)
  // float and double are *not* associative, and ScalaCheck knows
  //checkAll("Double", GroupTests[Double].group)
  //checkAll("Float", GroupTests[Float].group)
  checkAll("Long", GroupTests[Long].group)
}
