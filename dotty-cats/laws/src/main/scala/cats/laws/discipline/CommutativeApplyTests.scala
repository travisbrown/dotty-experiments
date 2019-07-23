package io.circe.cats.laws.discipline

import io.circe.cats.CommutativeApply
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.CommutativeApplyLaws
import io.circe.cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait CommutativeApplyTests[F[_]] extends ApplyTests[F] {
  def laws: CommutativeApplyLaws[F]

  def commutativeApply[A, B, C] given Arbitrary[A], Arbitrary[B], Arbitrary[C],
    Eq[A], Eq[B], Eq[C],
    Arbitrary[F[A]], Arbitrary[F[B]], Arbitrary[F[C]], Arbitrary[F[A => B]], Arbitrary[F[B => C]],
    Cogen[A], Cogen[B], Cogen[C],
    Eq[F[A]], Eq[F[C]], Eq[F[(A, B, C)]], Isomorphisms[F]: RuleSet =
    new RuleSet {
      def name: String = "commutative apply"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(apply[A, B, C])
      def props: Seq[(String, Prop)] = Seq(
        "apply commutativity" -> Prop.forAll(laws.applyCommutative[A, B, C])
      )
    }

}

object CommutativeApplyTests {
  def apply[F[_]] given CommutativeApply[F]: CommutativeApplyTests[F] =
    new CommutativeApplyTests[F] {
      def laws: CommutativeApplyLaws[F] = CommutativeApplyLaws[F]
    }
}
