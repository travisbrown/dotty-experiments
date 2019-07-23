package io.circe.cats.laws.discipline

import io.circe.cats.Bifunctor
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.BifunctorLaws
import org.scalacheck.{Arbitrary, Cogen, Prop}
import org.typelevel.discipline.Laws

trait BifunctorTests[F[_, _]] extends Laws {
  def laws: BifunctorLaws[F]

  def bifunctor[A, A2, A3, B, B2, B3] given Arbitrary[F[A, B]], Arbitrary[A => A2], Arbitrary[A2 => A3], Arbitrary[B => B2], Arbitrary[B2 => B3],
    Eq[F[A, B]], Eq[F[A3, B3]], Eq[F[A3, B]], Eq[F[A, B3]]: RuleSet =
    new DefaultRuleSet(
      name = "Bifunctor",
      parent = None,
      "Bifunctor Identity" -> Prop.forAll(laws.bifunctorIdentity[A, B]),
      "Bifunctor associativity" -> Prop.forAll(laws.bifunctorComposition[A, A2, A3, B, B2, B3]),
      "Bifunctor leftMap Identity" -> Prop.forAll(laws.bifunctorLeftMapIdentity[A, B]),
      "Bifunctor leftMap associativity" -> Prop.forAll(laws.bifunctorLeftMapComposition[A, B, A2, A3])
    )
}

object BifunctorTests {
  def apply[F[_, _]] given Bifunctor[F]: BifunctorTests[F] =
    new BifunctorTests[F] {
      def laws: BifunctorLaws[F] = BifunctorLaws[F]
    }
}
