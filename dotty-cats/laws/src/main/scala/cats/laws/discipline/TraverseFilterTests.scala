package io.circe.cats.laws.discipline

import io.circe.cats.TraverseFilter
import io.circe.cats.data.Nested
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.TraverseFilterLaws
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait TraverseFilterTests[F[_]] extends FunctorFilterTests[F] {
  def laws: TraverseFilterLaws[F]

  def traverseFilter[A, B, C] given Arbitrary[F[A]],
    Arbitrary[F[Option[A]]], Arbitrary[PartialFunction[A, B]], Arbitrary[A => Option[B]], Arbitrary[A => Option[A]], Arbitrary[A => Option[Option[B]]],
    Arbitrary[B => Option[C]], Arbitrary[B => Option[Option[C]]], Arbitrary[A => B], Arbitrary[A => Boolean], Arbitrary[A => Option[Boolean]],
    Eq[F[A]], Eq[F[B]], Eq[F[C]], Eq[Option[F[A]]], Eq[Nested[Option, Option, F[C]]]: RuleSet =
    new DefaultRuleSet(
      name = "traverseFilter",
      parent = Some(functorFilter[A, B, C]),
      "traverseFilter identity" -> Prop.forAll(laws.traverseFilterIdentity[Option, A]),
      "traverseFilter nested composition" -> Prop.forAll(laws.traverseFilterComposition[A, B, C, Option, Option]),
      "traverseFilter consistent with traverse" -> Prop.forAll(laws.traverseFilterConsistentWithTraverse[Option, A]),
      "filterA consistent with traverseFilter" -> Prop.forAll(laws.filterAConsistentWithTraverseFilter[Option, A])
    )
}

object TraverseFilterTests {
  def apply[F[_]] given TraverseFilter[F]: TraverseFilterTests[F] =
    new TraverseFilterTests[F] { def laws: TraverseFilterLaws[F] = TraverseFilterLaws[F] }
}
