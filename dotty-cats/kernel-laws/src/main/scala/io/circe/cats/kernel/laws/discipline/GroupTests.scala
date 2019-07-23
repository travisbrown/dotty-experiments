package io.circe.cats.kernel.laws.discipline

import io.circe.cats.kernel.{Eq, Group}
import io.circe.cats.kernel.laws.GroupLaws
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait GroupTests[A] extends MonoidTests[A] {
  def laws: GroupLaws[A]

  def group given Arbitrary[A], Eq[A]: RuleSet =
    new DefaultRuleSet(
      "group",
      Some(monoid),
      "left inverse" -> forAll(laws.leftInverse),
      "right inverse" -> forAll(laws.rightInverse),
      "consistent inverse" -> forAll(laws.consistentInverse)
    )
}

object GroupTests {
  def apply[A] given Group[A]: GroupTests[A] =
    new GroupTests[A] { def laws: GroupLaws[A] = GroupLaws[A] }
}
