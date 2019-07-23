package io.circe.cats.laws.discipline

import io.circe.cats.arrow.Choice
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.ChoiceLaws
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait ChoiceTests[F[_, _]] extends CategoryTests[F] {
  def laws: ChoiceLaws[F]

  def choice[A, B, C, D] given Arbitrary[F[A, B]], Arbitrary[F[A, C]], Arbitrary[F[B, C]], Arbitrary[F[C, D]], Eq[F[A, B]], Eq[F[A, D]], Eq[F[Either[A, B], D]]: RuleSet =
    new DefaultRuleSet(
      name = "choice",
      parent = Some(category[A, B, C, D]),
      "choice composition distributivity" -> Prop.forAll(laws.choiceCompositionDistributivity[A, B, C, D])
    )
}

object ChoiceTests {
  def apply[F[_, _]] given Choice[F]: ChoiceTests[F] =
    new ChoiceTests[F] { def laws: ChoiceLaws[F] = ChoiceLaws[F] }
}
