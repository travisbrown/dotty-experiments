package io.circe.cats.laws.discipline

import io.circe.cats.ContravariantSemigroupal
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.ContravariantSemigroupalLaws
import io.circe.cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait ContravariantSemigroupalTests[F[_]] extends ContravariantTests[F] with SemigroupalTests[F] {
  def laws: ContravariantSemigroupalLaws[F]

  def contravariantSemigroupal[A, B, C] given Arbitrary[A], Arbitrary[B], Arbitrary[C],
    Arbitrary[F[A]], Arbitrary[F[B]], Arbitrary[F[C]],
    Cogen[A], Cogen[B], Cogen[C],
    Eq[F[A]], Eq[F[B]], Eq[F[C]],
    Eq[F[(A, B, C)]],
    Isomorphisms[F]: RuleSet =
    new RuleSet {
      val name = "contravariantSemigroupal"
      val parents = Seq(contravariant[A, B, C], semigroupal[A, B, C])
      val bases = Seq.empty
      val props = Seq(
        "contravariantSemigroupal contramap2 delta associates" ->
          Prop.forAll(laws.contravariantSemigroupalContramap2DiagonalAssociates[A])
      )
    }
}

object ContravariantSemigroupalTests {
  def apply[F[_]] given ContravariantSemigroupal[F]: ContravariantSemigroupalTests[F] =
    new ContravariantSemigroupalTests[F] { def laws: ContravariantSemigroupalLaws[F] = ContravariantSemigroupalLaws[F] }
}
