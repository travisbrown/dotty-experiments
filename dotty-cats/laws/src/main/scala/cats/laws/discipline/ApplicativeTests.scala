package io.circe.cats.laws.discipline

import io.circe.cats.Applicative
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.ApplicativeLaws
import io.circe.cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait ApplicativeTests[F[_]] extends ApplyTests[F] {
  def laws: ApplicativeLaws[F]

  def applicative[A, B, C] given Arbitrary[A], Arbitrary[B], Arbitrary[C],
    Arbitrary[F[A]], Arbitrary[F[B]], Arbitrary[F[C]],
    Arbitrary[F[A => B]], Arbitrary[F[B => C]],
    Cogen[A], Cogen[B], Cogen[C],
    Eq[F[A]], Eq[F[B]], Eq[F[C]], Eq[F[(A, B, C)]], Isomorphisms[F]: RuleSet =
    new DefaultRuleSet(
      name = "applicative",
      parent = Some(apply[A, B, C]),
      "applicative identity" -> Prop.forAll(laws.applicativeIdentity[A]),
      "applicative homomorphism" -> Prop.forAll(laws.applicativeHomomorphism[A, B]),
      "applicative interchange" -> Prop.forAll(laws.applicativeInterchange[A, B]),
      "applicative map" -> Prop.forAll(laws.applicativeMap[A, B]),
      "applicative unit" -> Prop.forAll(laws.applicativeUnit[A]),
      "ap consistent with product + map" -> Prop.forAll(laws.apProductConsistent[A, B]),
      "monoidal left identity" -> Prop.forAll((fa: F[A]) => the[Isomorphisms[F]].leftIdentity(laws.monoidalLeftIdentity(fa))),
      "monoidal right identity" -> Prop.forAll((fa: F[A]) => the[Isomorphisms[F]].rightIdentity(laws.monoidalRightIdentity(fa)))
    )
}

object ApplicativeTests {
  def apply[F[_]] given Applicative[F]: ApplicativeTests[F] =
    new ApplicativeTests[F] { def laws: ApplicativeLaws[F] = ApplicativeLaws[F] }
}
