package io.circe.cats.laws.discipline

import io.circe.cats.Bifoldable
import io.circe.cats.kernel.{Eq, Monoid}
import io.circe.cats.laws.BifoldableLaws
import org.scalacheck.{Arbitrary, Cogen, Prop}
import org.typelevel.discipline.Laws

trait BifoldableTests[F[_, _]] extends Laws {
  def laws: BifoldableLaws[F]

  def bifoldable[A, B, C] given Arbitrary[A], Arbitrary[B], Arbitrary[C], Monoid[C], Eq[C],
    Arbitrary[F[A, B]], Cogen[A], Cogen[B]: RuleSet =
    new DefaultRuleSet(
      name = "bifoldable",
      parent = None,
      "bifoldLeft consistent with bifoldMap" -> Prop.forAll(laws.bifoldLeftConsistentWithBifoldMap[A, B, C]),
      "bifoldRight consistent with bifoldMap" -> Prop.forAll(laws.bifoldRightConsistentWithBifoldMap[A, B, C])
    )
}

object BifoldableTests {
  def apply[F[_, _]] given Bifoldable[F]: BifoldableTests[F] =
    new BifoldableTests[F] { def laws: BifoldableLaws[F] = BifoldableLaws[F] }
}
