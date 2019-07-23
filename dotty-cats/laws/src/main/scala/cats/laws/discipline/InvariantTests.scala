package io.circe.cats.laws.discipline

import io.circe.cats.Invariant
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.InvariantLaws
import org.scalacheck.{Arbitrary, Cogen, Prop}
import org.typelevel.discipline.Laws

trait InvariantTests[F[_]] extends Laws {
  def laws: InvariantLaws[F]

  def invariant[A, B, C] given Arbitrary[A], Arbitrary[B], Arbitrary[C], Arbitrary[F[A]], Cogen[A], Cogen[B], Cogen[C], Eq[F[A]], Eq[F[C]]: RuleSet =
    new DefaultRuleSet(
      name = "invariant",
      parent = None,
      "invariant identity" -> Prop.forAll(laws.invariantIdentity[A]),
      "invariant composition" -> Prop.forAll(laws.invariantComposition[A, B, C])
    )
}

object InvariantTests {
  def apply[F[_]] given Invariant[F]: InvariantTests[F] =
    new InvariantTests[F] { def laws: InvariantLaws[F] = InvariantLaws[F] }
}
