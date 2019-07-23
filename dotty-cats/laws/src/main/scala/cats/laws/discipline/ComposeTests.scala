package io.circe.cats.laws.discipline

import io.circe.cats.arrow.Compose
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.ComposeLaws
import org.scalacheck.{Arbitrary, Cogen, Prop}
import org.typelevel.discipline.Laws

trait ComposeTests[F[_, _]] extends Laws {
  def laws: ComposeLaws[F]

  def compose[A, B, C, D] given Arbitrary[F[A, B]], Arbitrary[F[B, C]], Arbitrary[F[C, D]], Eq[F[A, D]]: RuleSet =
    new DefaultRuleSet(name = "compose",
                       parent = None,
                       "compose associativity" -> Prop.forAll(laws.composeAssociativity[A, B, C, D]))
}

object ComposeTests {
  def apply[F[_, _]] given Compose[F]: ComposeTests[F] =
    new ComposeTests[F] { def laws: ComposeLaws[F] = ComposeLaws[F] }
}
