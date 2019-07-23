package io.circe.cats.laws.discipline

import io.circe.cats.MonadError
import io.circe.cats.data.EitherT
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.MonadErrorLaws
//import io.circe.cats.laws.discipline.arbitrary._
import io.circe.cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait MonadErrorTests[F[_], E] extends ApplicativeErrorTests[F, E] with MonadTests[F] {
  def laws: MonadErrorLaws[F, E]

  def monadError[A, B, C] given Arbitrary[A], Arbitrary[B], Arbitrary[C], Arbitrary[E],
    Arbitrary[F[A]], Arbitrary[F[B]], Arbitrary[F[C]],
    Arbitrary[F[A => B]], Arbitrary[F[B => C]],
    Arbitrary[F[Unit]],
    Arbitrary[E => F[Unit]],
    Cogen[A], Cogen[B], Cogen[C], Cogen[E],
    Eq[A], Eq[B], Eq[C], Eq[E],
    Eq[F[A]], Eq[F[B]], Eq[F[C]],
    Eq[F[Either[E, Unit]]], Eq[F[Either[E, A]]], Eq[EitherT[F, E, A]],
    Eq[F[(A, B, C)]],
    Eq[F[Int]],
    Isomorphisms[F]: RuleSet =
    new RuleSet {
      def name: String = "monadError"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(applicativeError[A, B, C], monad[A, B, C])
      def props: Seq[(String, Prop)] = Seq(
        "monadError left zero" -> Prop.forAll(laws.monadErrorLeftZero[A, B]),
        "monadError ensure consistency" -> Prop.forAll(laws.monadErrorEnsureConsistency[A]),
        "monadError ensureOr consistency" -> Prop.forAll(laws.monadErrorEnsureOrConsistency[A]),
        "monadError adaptError pure" -> Prop.forAll(laws.adaptErrorPure[A]),
        "monadError adaptError raise" -> Prop.forAll(laws.adaptErrorRaise[A]),
        "monadError rethrow attempt" -> Prop.forAll(laws.rethrowAttempt[A])
      )
    }
}

object MonadErrorTests {
  def apply[F[_], E] given MonadError[F, E]: MonadErrorTests[F, E] =
    new MonadErrorTests[F, E] {
      def laws: MonadErrorLaws[F, E] = MonadErrorLaws[F, E]
    }
}
