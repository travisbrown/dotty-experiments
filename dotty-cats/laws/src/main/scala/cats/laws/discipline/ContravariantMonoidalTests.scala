package io.circe.cats.laws.discipline

import io.circe.cats.ContravariantMonoidal
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.ContravariantMonoidalLaws
import io.circe.cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait ContravariantMonoidalTests[F[_]] extends ContravariantSemigroupalTests[F] {
  def laws: ContravariantMonoidalLaws[F]

  def contravariantMonoidal[A, B, C] given Arbitrary[A], Arbitrary[B], Arbitrary[C],
    Arbitrary[F[A]], Arbitrary[F[B]], Arbitrary[F[C]],
    Cogen[A], Cogen[B], Cogen[C],
    Eq[F[A]], Eq[F[B]], Eq[F[C]],
    Eq[F[(A, B, C)]],
    Isomorphisms[F]: RuleSet =
    new RuleSet {
      val name = "contravariantMonoidal"
      val parents = Seq(contravariantSemigroupal[A, B, C])
      val bases = Seq.empty
      val props = Seq(
        "contravariantMonoidal right unit" ->
          Prop.forAll(laws.contravariantMonoidalUnitRight[A]),
        "contravariantMonoidal left unit" ->
          Prop.forAll(laws.contravariantMonoidalUnitLeft[A]),
        "contravariantMonoidal contramap2 compatible contramap left" ->
          Prop.forAll(laws.contravariantMonoidalContramap2CompatibleContramapLeft[A, B, C]),
        "contravariantMonoidal contramap2 compatible contramap right" ->
          Prop.forAll(laws.contravariantMonoidalContramap2CompatibleContramapRight[A, B, C])
      )
    }
}

object ContravariantMonoidalTests {
  def apply[F[_]] given ContravariantMonoidal[F]: ContravariantMonoidalTests[F] =
    new ContravariantMonoidalTests[F] { def laws: ContravariantMonoidalLaws[F] = ContravariantMonoidalLaws[F] }
}
