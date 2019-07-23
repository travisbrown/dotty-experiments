package io.circe.cats.laws.discipline

import io.circe.cats.ApplicativeError
import io.circe.cats.data.EitherT
import io.circe.cats.kernel.Eq
import io.circe.cats.laws.ApplicativeErrorLaws
import io.circe.cats.laws.discipline.arbitrary._
import io.circe.cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait ApplicativeErrorTests[F[_], E] extends ApplicativeTests[F] {
  def laws: ApplicativeErrorLaws[F, E]

  def applicativeError[A, B, C] given Arbitrary[A], Arbitrary[B], Arbitrary[C], Arbitrary[E],
    Arbitrary[F[A]], Arbitrary[F[B]], Arbitrary[F[C]],
    Arbitrary[F[A => B]], Arbitrary[F[B => C]],
    Arbitrary[F[Unit]],
    Arbitrary[E => F[Unit]],
    Cogen[A], Cogen[B], Cogen[C], Cogen[E],
    Eq[A], Eq[B], Eq[C], Eq[E],
    Eq[F[A]], Eq[F[B]], Eq[F[C]],
    Eq[F[Either[E, Unit]]], Eq[F[Either[E, A]]], Eq[EitherT[F, E, A]],
    Eq[F[(A, B, C)]],
    Isomorphisms[F]: RuleSet =
    new RuleSet {
      def name: String = "applicativeError"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(applicative[A, B, C])
      def props: Seq[(String, Prop)] = Seq(
        "applicativeError handleWith" -> Prop.forAll(laws.applicativeErrorHandleWith[A]),
        "applicativeError handle" -> Prop.forAll(laws.applicativeErrorHandle[A]),
        "applicativeError handleErrorWith pure" -> Prop.forAll(laws.handleErrorWithPure[A]),
        "applicativeError handleError pure" -> Prop.forAll(laws.handleErrorPure[A]),
        "applicativeError raiseError attempt" -> Prop.forAll(laws.raiseErrorAttempt),
        "applicativeError pure attempt" -> Prop.forAll(laws.pureAttempt[A]),
        "applicativeError handleErrorWith consistent with recoverWith" -> Prop.forAll(
          laws.handleErrorWithConsistentWithRecoverWith[A]
        ),
        "applicativeError handleError consistent with recover" -> Prop.forAll(laws.handleErrorConsistentWithRecover[A]),
        "applicativeError recover consistent with recoverWith" -> Prop.forAll(laws.recoverConsistentWithRecoverWith[A]),
        "applicativeError attempt consistent with attemptT" -> Prop.forAll(laws.attemptConsistentWithAttemptT[A]),
        "applicativeError attempt fromEither consistent with pure" -> Prop.forAll(
          laws.attemptFromEitherConsistentWithPure[A]
        ),
        "applicativeError onError pure" -> Prop.forAll(laws.onErrorPure[A]),
        "applicativeError onError raise" -> Prop.forAll(laws.onErrorRaise[A])
      )
    }
}

object ApplicativeErrorTests {
  def apply[F[_], E] given ApplicativeError[F, E]: ApplicativeErrorTests[F, E] =
    new ApplicativeErrorTests[F, E] {
      def laws: ApplicativeErrorLaws[F, E] = ApplicativeErrorLaws[F, E]
    }
}
