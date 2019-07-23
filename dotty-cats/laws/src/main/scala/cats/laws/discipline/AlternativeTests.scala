package io.circe.cats.laws.discipline

import io.circe.cats.Alternative
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.AlternativeLaws
import io.circe.cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait AlternativeTests[F[_]] extends ApplicativeTests[F] with MonoidKTests[F] {
  def laws: AlternativeLaws[F]

  def alternative[A, B, C] given Arbitrary[A], Arbitrary[B], Arbitrary[C],
    Arbitrary[F[A]], Arbitrary[F[B]], Arbitrary[F[C]],
    Arbitrary[F[A => B]], Arbitrary[F[B => C]],
    Cogen[A], Cogen[B], Cogen[C],
    Eq[F[A]], Eq[F[B]], Eq[F[C]], Eq[F[(A, B, C)]], Isomorphisms[F]: RuleSet =
    new RuleSet {
      val name: String = "alternative"
      val bases: Seq[(String, RuleSet)] = Nil
      val parents: Seq[RuleSet] = Seq(monoidK[A], applicative[A, B, C])
      val props: Seq[(String, Prop)] = Seq(
        "left distributivity" -> Prop.forAll(laws.alternativeLeftDistributivity[A, B]),
        "right distributivity" -> Prop.forAll(laws.alternativeRightDistributivity[A, B]),
        "right absorption" -> Prop.forAll(laws.alternativeRightAbsorption[A, B])
      )
    }

}

object AlternativeTests {
  def apply[F[_]] given Alternative[F]: AlternativeTests[F] =
    new AlternativeTests[F] { def laws: AlternativeLaws[F] = AlternativeLaws[F] }
}
