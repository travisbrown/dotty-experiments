package io.circe.cats.laws.discipline

import io.circe.cats.Comonad
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.ComonadLaws
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait ComonadTests[F[_]] extends CoflatMapTests[F] {
  def laws: ComonadLaws[F]

  def comonad[A, B, C] given Arbitrary[A], Arbitrary[B], Arbitrary[C], Arbitrary[F[A]],
    Eq[B],
    Cogen[A], Cogen[B], Cogen[C],
    Cogen[F[A]], Cogen[F[B]],
    Eq[F[A]], Eq[F[B]], Eq[F[C]], Eq[F[F[A]]], Eq[F[F[F[A]]]]: RuleSet =
    new DefaultRuleSet(
      name = "comonad",
      parent = Some(coflatMap[A, B, C]),
      "extractCoflattenIdentity" -> Prop.forAll(laws.extractCoflattenIdentity[A]),
      "mapCoflattenIdentity" -> Prop.forAll(laws.mapCoflattenIdentity[A]),
      "coflattenThroughMap" -> Prop.forAll(laws.coflattenThroughMap[A]),
      "coflattenCoherence" -> Prop.forAll(laws.coflattenCoherence[A, B]),
      "coflatMapIdentity" -> Prop.forAll(laws.coflatMapIdentity[A, B]),
      "mapCoflatMapCoherence" -> Prop.forAll(laws.mapCoflatMapCoherence[A, B]),
      "comonad left identity" -> Prop.forAll(laws.comonadLeftIdentity[A]),
      "comonad right identity" -> Prop.forAll(laws.comonadRightIdentity[A, B])
    )
}

object ComonadTests {
  def apply[F[_]] given Comonad[F]: ComonadTests[F] =
    new ComonadTests[F] {
      def laws: ComonadLaws[F] = ComonadLaws[F]
    }
}
