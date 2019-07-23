package io.circe.cats.laws.discipline

import io.circe.cats.Functor
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.FunctorLaws
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait FunctorTests[F[_]] extends InvariantTests[F] {
  def laws: FunctorLaws[F]

  def functor[A, B, C] given Arbitrary[A], Arbitrary[B], Arbitrary[C], Arbitrary[F[A]], Cogen[A], Cogen[B], Cogen[C], Eq[F[A]], Eq[F[C]]: RuleSet =
    new DefaultRuleSet(
      name = "functor",
      parent = Some(invariant[A, B, C]),
      "covariant identity" -> Prop.forAll(laws.covariantIdentity[A]),
      "covariant composition" -> Prop.forAll(laws.covariantComposition[A, B, C])
    )
}

object FunctorTests {
  def apply[F[_]] given Functor[F]: FunctorTests[F] =
    new FunctorTests[F] { def laws: FunctorLaws[F] = FunctorLaws[F] }
}
