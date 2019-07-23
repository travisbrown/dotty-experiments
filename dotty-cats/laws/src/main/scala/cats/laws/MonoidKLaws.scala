package io.circe.cats.laws

import io.circe.cats.MonoidK
import io.circe.cats.data.AndThen
import io.circe.cats.kernel.laws.IsEq

/**
 * Laws that must be obeyed by any `cats.MonoidK`.
 */
trait MonoidKLaws[F[_]] given (F: MonoidK[F]) extends SemigroupKLaws[F] {
  def monoidKLeftIdentity[A](a: F[A]): IsEq[F[A]] =
    F.combineK(F.empty, a) <-> a

  def monoidKRightIdentity[A](a: F[A]): IsEq[F[A]] =
    F.combineK(a, F.empty) <-> a
}

object MonoidKLaws {
  def apply[F[_]] given MonoidK[F]: MonoidKLaws[F] =
    new MonoidKLaws[F] with SemigroupKLaws[F]
}
