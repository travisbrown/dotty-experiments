package io.circe.cats.kernel

import io.circe.cats.{Contravariant, Invariant}
import given io.circe.cats.syntax.hash._
import io.circe.cats.tests.CatsSuite

object HashSuite extends CatsSuite {
  {
    Invariant[Hash]
    Contravariant[Hash]
  }

  test("hash syntax") {
    assert(1.hash == 1.hashCode)
    assert("ABC".hash == "ABC".hashCode)
  }
}
