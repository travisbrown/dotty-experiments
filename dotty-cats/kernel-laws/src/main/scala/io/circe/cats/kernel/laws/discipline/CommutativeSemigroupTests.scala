package io.circe.cats.kernel.laws.discipline

import io.circe.cats.kernel.{ CommutativeSemigroup, Eq }
import io.circe.cats.kernel.laws.CommutativeSemigroupLaws
import org.scalacheck.{Arbitrary, Prop}

trait CommutativeSemigroupTests[A] extends SemigroupTests[A] {

  def laws: CommutativeSemigroupLaws[A]

  def commutativeSemigroup given Arbitrary[A], Eq[A]: RuleSet =
    new DefaultRuleSet("commutativeSemigroup", Some(semigroup), "commutative" -> Prop.forAll(laws.commutative))

}

object CommutativeSemigroupTests {
  def apply[A] given CommutativeSemigroup[A]: CommutativeSemigroupTests[A] =
    new CommutativeSemigroupTests[A] { def laws: CommutativeSemigroupLaws[A] = CommutativeSemigroupLaws[A] }
}
