package io.circe.cats.laws.discipline

import io.circe.cats.InvariantMonoidal
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.InvariantMonoidalLaws
import io.circe.cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait InvariantMonoidalTests[F[_]] extends InvariantSemigroupalTests[F] {
  def laws: InvariantMonoidalLaws[F]

  def invariantMonoidal[A, B, C] given Arbitrary[A], Arbitrary[B], Arbitrary[C],
    Arbitrary[F[A]], Arbitrary[F[B]], Arbitrary[F[C]],
    Cogen[A], Cogen[B], Cogen[C],
    Eq[F[(A, (B, C))]], Eq[F[(A, B, C)]],
    Eq[F[A]], Eq[F[B]], Eq[F[C]],
    Isomorphisms[F]: RuleSet =
    new RuleSet {
      val name = "invariantMonoidal"
      val parents = Seq(invariantSemigroupal[A, B, C])
      val bases = Seq.empty
      val props = Seq(
        "invariant monoidal left identity" -> Prop.forAll((fa: F[A]) => laws.invariantMonoidalLeftIdentity(fa)),
        "invariant monoidal right identity" -> Prop.forAll((fa: F[A]) => laws.invariantMonoidalRightIdentity(fa))
      )
    }
}

object InvariantMonoidalTests {
  def apply[F[_]] given InvariantMonoidal[F]: InvariantMonoidalTests[F] =
    new InvariantMonoidalTests[F] { def laws: InvariantMonoidalLaws[F] = InvariantMonoidalLaws[F] }
}
