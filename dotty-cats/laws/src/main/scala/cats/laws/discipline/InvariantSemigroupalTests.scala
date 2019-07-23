package io.circe.cats.laws.discipline

import io.circe.cats.InvariantSemigroupal
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.InvariantSemigroupalLaws
import io.circe.cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait InvariantSemigroupalTests[F[_]] extends InvariantTests[F] with SemigroupalTests[F] {
  def laws: InvariantSemigroupalLaws[F]

  def invariantSemigroupal[A, B, C] given Arbitrary[A], Arbitrary[B], Arbitrary[C],
    Arbitrary[F[A]], Arbitrary[F[B]], Arbitrary[F[C]],
    Cogen[A], Cogen[B], Cogen[C],
    Eq[F[(A, (B, C))]], Eq[F[(A, B, C)]],
    Eq[F[A]], Eq[F[B]], Eq[F[C]],
    Isomorphisms[F]: RuleSet =
    new RuleSet {
      val name = "invariantSemigroupal"
      val parents = Seq(invariant[A, B, C], semigroupal[A, B, C])
      val bases = Nil
      val props = Seq(
        "invariant semigroupal associativity" -> Prop.forAll(
          (fa: F[A], fb: F[B], fc: F[C]) => laws.invariantSemigroupalAssociativity(fa, fb, fc)
        )
      )
    }
}

object InvariantSemigroupalTests {
  def apply[F[_]] given InvariantSemigroupal[F]: InvariantSemigroupalTests[F] =
    new InvariantSemigroupalTests[F] { def laws: InvariantSemigroupalLaws[F] = InvariantSemigroupalLaws[F] }
}
