package io.circe.cats.kernel.laws.discipline

import io.circe.cats.kernel.{Band, Eq}
import io.circe.cats.kernel.laws.BandLaws
import org.scalacheck.{Arbitrary, Prop}

trait BandTests[A] extends SemigroupTests[A] {

  def laws: BandLaws[A]

  def band given Arbitrary[A], Eq[A]: RuleSet =
    new DefaultRuleSet("band", Some(semigroup), "idempotency" -> Prop.forAll(laws.idempotence))
}

object BandTests {
  def apply[A] given Band[A]: BandTests[A] =
    new BandTests[A] { def laws: BandLaws[A] = BandLaws[A] }
}
