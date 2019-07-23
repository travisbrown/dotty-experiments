package io.circe.cats.laws.discipline

import io.circe.cats.FunctorFilter
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.FunctorFilterLaws
import org.scalacheck.{Arbitrary, Cogen, Prop}
import org.typelevel.discipline.Laws

trait FunctorFilterTests[F[_]] extends Laws {
  def laws: FunctorFilterLaws[F]

  def functorFilter[A, B, C] given Arbitrary[F[A]],
    Arbitrary[PartialFunction[A, B]], Arbitrary[F[Option[A]]], Arbitrary[A => Option[B]], Arbitrary[B => Option[C]], Arbitrary[A => B], Arbitrary[A => Boolean],
    Eq[F[A]], Eq[F[B]], Eq[F[C]]: RuleSet =
    new DefaultRuleSet(
      name = "functorFilter",
      parent = None,
      "mapFilter composition" -> Prop.forAll(laws.mapFilterComposition[A, B, C]),
      "mapFilter map consistency" -> Prop.forAll(laws.mapFilterMapConsistency[A, B]),
      "collect mapFilter consistency" -> Prop.forAll(laws.collectConsistentWithMapFilter[A, B]),
      "flattenOption mapFilter consistency" -> Prop.forAll(laws.flattenOptionConsistentWithMapFilter[A]),
      "filter mapFilter consistency" -> Prop.forAll(laws.filterConsistentWithMapFilter[A])
    )
}

object FunctorFilterTests {
  def apply[F[_]] given FunctorFilter[F]: FunctorFilterTests[F] =
    new FunctorFilterTests[F] { def laws: FunctorFilterLaws[F] = FunctorFilterLaws[F] }
}
