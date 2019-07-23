package io.circe.cats.laws.discipline

import io.circe.cats.{Applicative, Reducible}
import io.circe.cats.kernel.{CommutativeMonoid, Eq}
import io.circe.cats.laws.ReducibleLaws
import io.circe.cats.laws.discipline.arbitrary._
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait ReducibleTests[F[_]] extends FoldableTests[F] {
  def laws: ReducibleLaws[F]

  def reducible[G[_], A, B] given Applicative[G], Arbitrary[A], Arbitrary[B],
    Arbitrary[F[A]], Arbitrary[F[B]], Arbitrary[F[G[A]]], Arbitrary[G[B]],
    Cogen[A], Cogen[B], Eq[A], Eq[B], Eq[F[A]], Eq[Option[A]], Eq[G[Unit]],
    CommutativeMonoid[A], CommutativeMonoid[B]: RuleSet =
    new DefaultRuleSet(
      name = "reducible",
      parent = Some(foldable[A, B]),
      "reduceLeftTo consistent with reduceMap" -> Prop.forAll(laws.reduceLeftToConsistentWithReduceMap[A, B]),
      "reduceRightTo consistent with reduceMap" -> Prop.forAll(laws.reduceRightToConsistentWithReduceMap[A, B]),
      "reduceRightTo consistent with reduceRightToOption" ->
        Prop.forAll(laws.reduceRightToConsistentWithReduceRightToOption[A, B]),
      "reduceRight consistent with reduceRightOption" ->
        Prop.forAll(laws.reduceRightConsistentWithReduceRightOption[A]),
      "reduce consistent with reduceLeft" ->
        Prop.forAll(laws.reduceReduceLeftConsistent[B]),
      "nonEmptyTraverse_ consistent with traverse_" -> Prop.forAll(laws.traverseConsistent[G, A, B]),
      "nonEmptySequence_ consistent with sequence_" -> Prop.forAll(laws.sequenceConsistent[G, A]),
      "size consistent with reduceMap" -> Prop.forAll(laws.sizeConsistent[A])
    )
}

object ReducibleTests {
  def apply[F[_]] given Reducible[F]: ReducibleTests[F] =
    new ReducibleTests[F] { def laws: ReducibleLaws[F] = ReducibleLaws[F] }
}
