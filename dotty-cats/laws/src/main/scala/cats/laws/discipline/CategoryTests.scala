package io.circe.cats.laws.discipline

import io.circe.cats.arrow.Category
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.CategoryLaws
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait CategoryTests[F[_, _]] extends ComposeTests[F] {
  def laws: CategoryLaws[F]

  def category[A, B, C, D] given Arbitrary[F[A, B]], Arbitrary[F[B, C]], Arbitrary[F[C, D]], Eq[F[A, B]], Eq[F[A, D]]: RuleSet =
    new DefaultRuleSet(
      name = "category",
      parent = Some(compose[A, B, C, D]),
      "category left identity" -> Prop.forAll(laws.categoryLeftIdentity[A, B]),
      "category right identity" -> Prop.forAll(laws.categoryRightIdentity[A, B])
    )
}

object CategoryTests {
  def apply[F[_, _]] given Category[F]: CategoryTests[F] =
    new CategoryTests[F] { def laws: CategoryLaws[F] = CategoryLaws[F] }
}
