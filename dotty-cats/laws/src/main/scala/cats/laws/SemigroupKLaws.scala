package io.circe.cats.laws

import io.circe.cats.SemigroupK
import io.circe.cats.kernel.laws.IsEq

/**
 * Laws that must be obeyed by any `cats.SemigroupK`.
 */
trait SemigroupKLaws[F[_]] given (F: SemigroupK[F]) {
  def semigroupKAssociative[A](a: F[A], b: F[A], c: F[A]): IsEq[F[A]] =
    F.combineK(F.combineK(a, b), c) <-> F.combineK(a, F.combineK(b, c))
}

object SemigroupKLaws {
  def apply[F[_]] given SemigroupK[F]: SemigroupKLaws[F] =
    new SemigroupKLaws[F] {}
}
