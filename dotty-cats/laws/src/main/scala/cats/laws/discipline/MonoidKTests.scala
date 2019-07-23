package io.circe.cats.laws.discipline

import io.circe.cats.MonoidK
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.MonoidKLaws
import org.scalacheck.{Arbitrary, Prop}

trait MonoidKTests[F[_]] extends SemigroupKTests[F] {
  def laws: MonoidKLaws[F]

  def monoidK[A] given Arbitrary[A], Arbitrary[F[A]], Eq[F[A]]: RuleSet =
    new DefaultRuleSet(
      "monoidK",
      Some(semigroupK[A]),
      "monoidK left identity" -> Prop.forAll(laws.monoidKLeftIdentity[A]),
      "monoidK right identity" -> Prop.forAll(laws.monoidKRightIdentity[A])
    )
}

object MonoidKTests {
  def apply[F[_]] given MonoidK[F]: MonoidKTests[F] =
    new MonoidKTests[F] { def laws: MonoidKLaws[F] = MonoidKLaws[F] }
}
