package io.circe.cats.kernel.laws.discipline

import io.circe.cats.kernel.{Eq, LowerBounded, UpperBounded}
import io.circe.cats.kernel.laws.{LowerBoundedLaws, UpperBoundedLaws}
import org.scalacheck.{Arbitrary, Prop}

trait LowerBoundedTests[A] extends PartialOrderTests[A] {
  def laws: LowerBoundedLaws[A]

  def lowerBounded given Arbitrary[A], Arbitrary[A => A], Eq[Option[A]], Eq[A]: RuleSet =
    new DefaultRuleSet(
      "lowerBounded",
      Some(partialOrder),
      "bound is less than or equals" -> Prop.forAll(laws.boundLteqv)
    )
}

object LowerBoundedTests {
  def apply[A] given (A: LowerBounded[A]): LowerBoundedTests[A] =
    new LowerBoundedTests[A] { def laws: LowerBoundedLaws[A] = LowerBoundedLaws[A] }
}

trait UpperBoundedTests[A] extends PartialOrderTests[A] {
  def laws: UpperBoundedLaws[A]

  def upperBounded given Arbitrary[A], Arbitrary[A => A], Eq[Option[A]], Eq[A]: RuleSet =
    new DefaultRuleSet(
      "upperBounded",
      Some(partialOrder),
      "bound is greater than or equals" -> Prop.forAll(laws.boundGteqv)
    )
}

object UpperBoundedTests {
  def apply[A] given (A: UpperBounded[A]): UpperBoundedTests[A] =
    new UpperBoundedTests[A] { def laws: UpperBoundedLaws[A] = UpperBoundedLaws[A] }
}
