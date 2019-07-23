package io.circe.cats.laws.discipline

import io.circe.cats.Contravariant
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.ContravariantLaws
import org.scalacheck.{Arbitrary, Cogen, Prop}
import org.typelevel.discipline.Laws

trait ContravariantTests[F[_]] extends InvariantTests[F] {
  def laws: ContravariantLaws[F]

  def contravariant[A, B, C] given Arbitrary[A], Arbitrary[B], Arbitrary[C], Arbitrary[F[A]], Cogen[A], Cogen[B], Cogen[C], Eq[F[A]], Eq[F[C]]: RuleSet =
    new DefaultRuleSet(
      name = "contravariant",
      parent = Some(invariant[A, B, C]),
      "contravariant identity" -> Prop.forAll(laws.contravariantIdentity[A]),
      "contravariant composition" -> Prop.forAll(laws.contravariantComposition[A, B, C])
    )
}

object ContravariantTests {
  def apply[F[_]] given Contravariant[F]: ContravariantTests[F] =
    new ContravariantTests[F] { def laws: ContravariantLaws[F] = ContravariantLaws[F] }
}
