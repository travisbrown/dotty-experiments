package io.circe.cats.laws

import io.circe.cats.InvariantSemigroupal
import io.circe.cats.kernel.laws.IsEq
import given io.circe.cats.syntax.invariant._
import given io.circe.cats.syntax.semigroupal._

/**
 * Laws that are expected for any `cats.InvariantSemigroupal`.
 */
trait InvariantSemigroupalLaws[F[_]] given (F: InvariantSemigroupal[F]) extends InvariantLaws[F] with SemigroupalLaws[F] {
  def invariantSemigroupalAssociativity[A, B, C](fa: F[A], fb: F[B], fc: F[C]): IsEq[F[(A, (B, C))]] =
    fa.product(fb.product(fc)) <-> fa
      .product(fb)
      .product(fc)
      .imap { case ((a, b), c) => (a, (b, c)) } { case (a, (b, c)) => ((a, b), c) }
}

object InvariantSemigroupalLaws {
  def apply[F[_]] given InvariantSemigroupal[F]: InvariantSemigroupalLaws[F] =
    new InvariantSemigroupalLaws[F] with InvariantLaws[F] with SemigroupalLaws[F]
}
