package io.circe.cats.laws.discipline

import io.circe.cats.Defer
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.DeferLaws
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

trait DeferTests[F[_]] extends Laws {
  def laws: DeferLaws[F]

  def defer[A] given Arbitrary[A], Arbitrary[F[A]], Eq[F[A]], Eq[Boolean]: RuleSet =
    new DefaultRuleSet(
      name = "defer",
      parent = None,
      "defer Identity" -> Prop.forAll(laws.deferIdentity[A]),
      "defer does not evaluate" -> Prop.forAll(laws.deferDoesNotEvaluate[A]),
      "defer is stack safe" -> Prop.forAll(laws.deferIsStackSafe[A])
    )
}

object DeferTests {
  def apply[F[_]] given Defer[F]: DeferTests[F] =
    new DeferTests[F] { def laws: DeferLaws[F] = DeferLaws[F] }
}
