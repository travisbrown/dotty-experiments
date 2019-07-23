package io.circe.cats.laws.discipline

import io.circe.cats.CommutativeMonad
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.CommutativeMonadLaws
import io.circe.cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait CommutativeMonadTests[F[_]]
    extends MonadTests[F]
    with CommutativeFlatMapTests[F]
    with CommutativeApplicativeTests[F] {
  def laws: CommutativeMonadLaws[F]

  def commutativeMonad[A, B, C] given Arbitrary[A], Arbitrary[B], Arbitrary[C],
    Eq[A], Eq[B], Eq[C],
    Arbitrary[F[A]], Arbitrary[F[B]], Arbitrary[F[C]],
    Arbitrary[F[A => B]], Arbitrary[F[B => C]],
    Cogen[A], Cogen[B], Cogen[C],
    Eq[F[A]], Eq[F[B]], Eq[F[C]],
    Eq[F[(A, B, C)]], Eq[F[Int]],
    Isomorphisms[F]: RuleSet =
    new RuleSet {
      def name: String = "commutative monad"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(monad[A, B, C], commutativeFlatMap[A, B, C], commutativeApplicative[A, B, C])
      def props: Seq[(String, Prop)] = Nil
    }

}

object CommutativeMonadTests {
  def apply[F[_]: CommutativeMonad]: CommutativeMonadTests[F] =
    new CommutativeMonadTests[F] {
      def laws: CommutativeMonadLaws[F] = CommutativeMonadLaws[F]
    }
}
