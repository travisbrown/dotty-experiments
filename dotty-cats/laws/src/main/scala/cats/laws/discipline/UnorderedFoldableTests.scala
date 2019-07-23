package io.circe.cats.laws.discipline

import io.circe.cats.UnorderedFoldable
import io.circe.cats.kernel.{CommutativeMonoid, Eq}
import io.circe.cats.laws.UnorderedFoldableLaws
import org.scalacheck.{Arbitrary, Cogen, Prop}
import org.typelevel.discipline.Laws

trait UnorderedFoldableTests[F[_]] extends Laws {
  def laws: UnorderedFoldableLaws[F]

  def unorderedFoldable[A, B] given Arbitrary[A], Arbitrary[B], Arbitrary[F[A]], Arbitrary[A => B], Cogen[A], CommutativeMonoid[A], CommutativeMonoid[B], Eq[A], Eq[B]: RuleSet =
    new DefaultRuleSet(
      name = "unorderedFoldable",
      parent = None,
      "unorderedFold consistent with unorderedFoldMap" -> Prop.forAll(laws.unorderedFoldConsistentWithUnorderedFoldMap[A]),
      "forall consistent with exists" -> Prop.forAll(laws.forallConsistentWithExists[A]),
      "forall true if empty" -> Prop.forAll(laws.forallEmpty[A]),
      "nonEmpty reference" -> Prop.forAll(laws.nonEmptyRef[A]),
      "exists is lazy" -> Prop.forAll(laws.existsLazy[A]),
      "forall is lazy" -> Prop.forAll(laws.forallLazy[A])
    )
}

object UnorderedFoldableTests {
  def apply[F[_]] given UnorderedFoldable[F]: UnorderedFoldableTests[F] =
    new UnorderedFoldableTests[F] { def laws: UnorderedFoldableLaws[F] = UnorderedFoldableLaws[F] }
}
