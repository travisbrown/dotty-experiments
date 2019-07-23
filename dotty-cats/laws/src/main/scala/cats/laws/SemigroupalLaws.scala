package io.circe.cats.laws

import io.circe.cats.Semigroupal

/**
 * Laws that must be obeyed by any `cats.Semigroupal`.
 */
trait SemigroupalLaws[F[_]] given (F: Semigroupal[F]) {
  def semigroupalAssociativity[A, B, C](fa: F[A], fb: F[B], fc: F[C]): (F[(A, (B, C))], F[((A, B), C)]) =
    (F.product(fa, F.product(fb, fc)), F.product(F.product(fa, fb), fc))
}

object SemigroupalLaws {
  def apply[F[_]] given Semigroupal[F]: SemigroupalLaws[F] =
    new SemigroupalLaws[F] {}
}
