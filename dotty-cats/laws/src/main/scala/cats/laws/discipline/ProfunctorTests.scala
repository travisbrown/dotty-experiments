package io.circe.cats.laws.discipline

import io.circe.cats.arrow.Profunctor
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.ProfunctorLaws
import org.scalacheck.{Arbitrary, Cogen, Prop}
import org.typelevel.discipline.Laws

trait ProfunctorTests[F[_, _]] extends Laws {
  def laws: ProfunctorLaws[F]

  def profunctor[A, B, C, D, E, G] given Arbitrary[A], Arbitrary[B], Arbitrary[C], Arbitrary[D], Arbitrary[E], Arbitrary[G], Arbitrary[F[A, B]], Arbitrary[F[C, D]], Cogen[A], Cogen[B], Cogen[C], Cogen[D], Cogen[E], Eq[F[A, B]], Eq[F[A, D]], Eq[F[A, G]]: RuleSet =
    new DefaultRuleSet(
      name = "profunctor",
      parent = None,
      "profunctor identity" -> Prop.forAll(laws.profunctorIdentity[A, B]),
      "profunctor composition" -> Prop.forAll(laws.profunctorComposition[A, B, C, D, E, G]),
      "profunctor lmap identity" -> Prop.forAll(laws.profunctorLmapIdentity[A, B]),
      "profunctor rmap identity" -> Prop.forAll(laws.profunctorRmapIdentity[A, B]),
      "profunctor lmap composition" -> Prop.forAll(laws.profunctorLmapComposition[A, B, C, D]),
      "profunctor rmap composition" -> Prop.forAll(laws.profunctorRmapComposition[A, D, C, B])
    )
}

object ProfunctorTests {
  def apply[F[_, _]] given Profunctor[F]: ProfunctorTests[F] =
    new ProfunctorTests[F] { def laws: ProfunctorLaws[F] = ProfunctorLaws[F] }
}
