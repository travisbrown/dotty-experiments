package io.circe.cats.kernel.laws.discipline

import io.circe.cats.kernel.Eq
import io.circe.cats.kernel.laws.EqLaws
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

trait EqTests[A] extends Laws {
  def laws: EqLaws[A]

  def eqv given Arbitrary[A], Arbitrary[A => A], Eq[A]: RuleSet =
    new DefaultRuleSet(
      "eq",
      None,
      "reflexivity" -> forAll(laws.reflexivityEq),
      "symmetry" -> forAll(laws.symmetryEq),
      "antisymmetry" -> forAll(laws.antiSymmetryEq),
      "transitivity" -> forAll(laws.transitivityEq)
    )
}

object EqTests {
  def apply[A] given Eq[A]: EqTests[A] =
    new EqTests[A] { def laws: EqLaws[A] = EqLaws[A] }
}
