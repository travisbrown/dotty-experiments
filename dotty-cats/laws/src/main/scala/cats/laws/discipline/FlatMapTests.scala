package io.circe.cats.laws.discipline

import io.circe.cats.{FlatMap, Functor}
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.FlatMapLaws
import io.circe.cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait FlatMapTests[F[_]] extends ApplyTests[F] {
  def laws: FlatMapLaws[F]

  def flatMap[A, B, C] given Arbitrary[A], Arbitrary[B], Arbitrary[C],
    Arbitrary[F[A]], Arbitrary[F[B]], Arbitrary[F[C]],
    Arbitrary[F[A => B]], Arbitrary[F[B => C]],
    Cogen[A], Cogen[B], Cogen[C],
    Eq[F[A]], Eq[F[B]], Eq[F[C]],
    Eq[F[(A, B, C)]],
    Isomorphisms[F]: RuleSet = {
    given as Eq[F[(A, B)]] =
      laws.eqTuple[A, B]

    new DefaultRuleSet(
      name = "flatMap",
      parent = Some(apply[A, B, C]),
      "flatMap associativity" -> Prop.forAll(laws.flatMapAssociativity[A, B, C]),
      "flatMap consistent apply" -> Prop.forAll(laws.flatMapConsistentApply[A, B]),
      "flatMap from tailRecM consistency" -> Prop.forAll(laws.flatMapFromTailRecMConsistency[A, B]),
      "mproduct consistent flatMap" -> Prop.forAll(laws.mproductConsistency[A, B]),
      "tailRecM consistent flatMap" -> Prop.forAll(laws.tailRecMConsistentFlatMap[A])
    )
  }
}

object FlatMapTests {
  def apply[F[_]] given FlatMap[F]: FlatMapTests[F] =
    new FlatMapTests[F] { def laws: FlatMapLaws[F] = FlatMapLaws[F] }
}
