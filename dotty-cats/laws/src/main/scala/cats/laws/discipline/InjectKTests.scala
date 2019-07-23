package io.circe.cats.laws.discipline

import io.circe.cats.InjectK
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.InjectKLaws
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

trait InjectKTests[F[_], G[_]] extends Laws {
  def laws: InjectKLaws[F, G]

  def injectK[A] given Arbitrary[F[A]], Arbitrary[G[A]], Eq[Option[F[A]]], Eq[Option[G[A]]]: RuleSet =
    new DefaultRuleSet(
      "injectK",
      None,
      "injectK round trip inj" -> Prop.forAll((fa: F[A]) => laws.injectKRoundTripInj(fa)),
      "injectK round trip prj" -> Prop.forAll((ga: G[A]) => laws.injectKRoundTripPrj(ga))
    )

}

object InjectKTests {
  def apply[F[_], G[_]] given InjectK[F, G]: InjectKTests[F, G] =
    new InjectKTests[F, G] { val laws: InjectKLaws[F, G] = InjectKLaws[F, G] }
}
