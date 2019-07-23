package io.circe.cats.laws.discipline

import io.circe.cats.Inject
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.InjectLaws
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

trait InjectTests[A, B] extends Laws {
  def laws: InjectLaws[A, B]

  def inject given Arbitrary[A], Arbitrary[B], Eq[Option[A]], Eq[Option[B]]: RuleSet =
    new DefaultRuleSet(
      "inject",
      None,
      "inject round trip inj" -> Prop.forAll((a: A) => laws.injectRoundTripInj(a)),
      "inject round trip prj" -> Prop.forAll((b: B) => laws.injectRoundTripPrj(b))
    )
}

object InjectTests {
  def apply[A, B] given Inject[A, B]: InjectTests[A, B] =
    new InjectTests[A, B] { val laws: InjectLaws[A, B] = InjectLaws[A, B] }
}
