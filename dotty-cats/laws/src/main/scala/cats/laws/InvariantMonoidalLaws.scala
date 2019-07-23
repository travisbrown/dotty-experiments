package io.circe.cats.laws

import io.circe.cats.InvariantMonoidal
import io.circe.cats.kernel.laws.IsEq
import given io.circe.cats.syntax.invariant._
import given io.circe.cats.syntax.semigroupal._

/**
 * Laws that must be obeyed by any `cats.InvariantMonoidal`.
 */
trait InvariantMonoidalLaws[F[_]] given (F: InvariantMonoidal[F]) extends InvariantSemigroupalLaws[F] {

  def invariantMonoidalLeftIdentity[A, B](fa: F[A]): IsEq[F[A]] =
    F.unit.product(fa).imap(_._2)(a => ((), a)) <-> fa

  def invariantMonoidalRightIdentity[A, B](fa: F[A]): IsEq[F[A]] =
    fa.product(F.unit).imap(_._1)(a => (a, ())) <-> fa
}

object InvariantMonoidalLaws {
  def apply[F[_]] given InvariantMonoidal[F]: InvariantMonoidalLaws[F] =
    new InvariantMonoidalLaws[F] with InvariantSemigroupalLaws[F] with InvariantLaws[F] with SemigroupalLaws[F]
}
