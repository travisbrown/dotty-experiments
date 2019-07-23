package io.circe.cats.laws.discipline

import io.circe.cats.CommutativeApplicative
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.CommutativeApplicativeLaws
import io.circe.cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait CommutativeApplicativeTests[F[_]] extends CommutativeApplyTests[F] with ApplicativeTests[F] {
  def laws: CommutativeApplicativeLaws[F]

  def commutativeApplicative[A, B, C] given Arbitrary[A], Arbitrary[B], Arbitrary[C],
    Arbitrary[F[A]], Arbitrary[F[B]], Arbitrary[F[C]],
    Arbitrary[F[A => B]], Arbitrary[F[B => C]],
    Cogen[A], Cogen[B], Cogen[C],
    Eq[A], Eq[B], Eq[C],
    Eq[F[A]], Eq[F[B]], Eq[F[C]], Eq[F[(A, B, C)]], Eq[F[Int]], Isomorphisms[F]: RuleSet =
    new RuleSet {
      def name: String = "commutative applicative"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(applicative[A, B, C], commutativeApply[A, B, C])
      def props: Seq[(String, Prop)] = Nil
    }
}

object CommutativeApplicativeTests {
  def apply[F[_]] given CommutativeApplicative[F]: CommutativeApplicativeTests[F] =
    new CommutativeApplicativeTests[F] {
      def laws: CommutativeApplicativeLaws[F] = CommutativeApplicativeLaws[F]
    }
}
