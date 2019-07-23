package io.circe.cats.kernel

import io.circe.cats.{Contravariant, ContravariantMonoidal, ContravariantSemigroupal, Invariant, Semigroupal}
import io.circe.cats.kernel.laws.discipline.SerializableTests
import io.circe.cats.laws.discipline.{ContravariantMonoidalTests, MiniInt}
import io.circe.cats.laws.discipline.arbitrary._
import io.circe.cats.laws.discipline.eq._
import io.circe.cats.tests.CatsSuite

object EqSuite extends CatsSuite {
  Invariant[Eq]
  Contravariant[Eq]
  Semigroupal[Eq]
  ContravariantSemigroupal[Eq]

  checkAll("Eq", ContravariantMonoidalTests[Eq].contravariantMonoidal[MiniInt, Boolean, Boolean])
  checkAll("ContravariantMonoidal[Eq]", SerializableTests.serializable(ContravariantMonoidal[Eq]))
}
