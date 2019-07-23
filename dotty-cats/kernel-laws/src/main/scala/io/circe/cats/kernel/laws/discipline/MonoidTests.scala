package io.circe.cats.kernel.laws.discipline

import io.circe.cats.kernel.{Eq, Monoid}
import io.circe.cats.kernel.laws.MonoidLaws
import org.scalacheck.{Arbitrary, Prop}

trait MonoidTests[A] extends SemigroupTests[A] {
  def laws: MonoidLaws[A]

  def monoid given Arbitrary[A], Eq[A]: RuleSet =
    new DefaultRuleSet(
      "monoid",
      Some(semigroup),
      "left identity" -> Prop.forAll(laws.leftIdentity),
      "right identity" -> Prop.forAll(laws.rightIdentity),
      "combine all" -> Prop.forAll(laws.combineAll),
      "collect0" -> Prop.forAll(laws.collect0),
      "is id" -> Prop.forAll((a: A) => laws.isId(a)),
      "repeat0" -> Prop.forAll(laws.repeat0)
    )

}

object MonoidTests {
  def apply[A] given Monoid[A]: MonoidTests[A] =
    new MonoidTests[A] { def laws: MonoidLaws[A] = MonoidLaws[A] }
}
