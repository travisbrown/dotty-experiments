package io.circe.cats.kernel.laws.discipline

import io.circe.cats.kernel.{Eq, PartialOrder}
import io.circe.cats.kernel.laws.PartialOrderLaws
import org.scalacheck.{Arbitrary, Prop}

trait PartialOrderTests[A] extends EqTests[A] {
  def laws: PartialOrderLaws[A]

  def partialOrder given Arbitrary[A], Arbitrary[A => A], Eq[Option[A]], Eq[A]: RuleSet =
    new DefaultRuleSet(
      "partialOrder",
      Some(eqv),
      "transitivity" -> Prop.forAll(laws.transitivity),
      "reflexivity lt" -> Prop.forAll(laws.reflexivityLt),
      "reflexivity gt" -> Prop.forAll(laws.reflexivityGt),
      "antisymmetry" -> Prop.forAll(laws.antisymmetry),
      "gt" -> Prop.forAll(laws.gt),
      "gteqv" -> Prop.forAll(laws.gteqv),
      "lt" -> Prop.forAll(laws.lt),
      "partialCompare" -> Prop.forAll(laws.partialCompare),
      "pmax" -> Prop.forAll(laws.pmax),
      "pmin" -> Prop.forAll(laws.pmin)
    )

}

object PartialOrderTests {
  def apply[A] given PartialOrder[A]: PartialOrderTests[A] =
    new PartialOrderTests[A] { def laws: PartialOrderLaws[A] = PartialOrderLaws[A] }
}
