package io.circe.cats.kernel.laws.discipline

import io.circe.cats.kernel.{ Eq, Semigroup }
import io.circe.cats.kernel.laws.SemigroupLaws
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

trait SemigroupTests[A] extends Laws {
  def laws: SemigroupLaws[A]

  def semigroup given Arbitrary[A], Eq[A]: RuleSet =
    new DefaultRuleSet(
      "semigroup",
      None,
      "associative" -> Prop.forAll(laws.semigroupAssociative),
      "repeat1" -> Prop.forAll(laws.repeat1),
      "repeat2" -> Prop.forAll(laws.repeat2),
      "combineAllOption" -> Prop.forAll(laws.combineAllOption)
    )
}

object SemigroupTests {
  def apply[A] given Semigroup[A]: SemigroupTests[A] =
    new SemigroupTests[A] { def laws: SemigroupLaws[A] = SemigroupLaws[A] }
}
