package io.circe.cats.laws

import io.circe.cats.arrow.Category
import io.circe.cats.kernel.laws.IsEq
import given io.circe.cats.syntax.compose._

/**
 * Laws that must be obeyed by any `cats.arrow.Category`.
 */
trait CategoryLaws[F[_, _]] given (F: Category[F]) extends ComposeLaws[F] {
  def categoryLeftIdentity[A, B](f: F[A, B]): IsEq[F[A, B]] =
    f <-> F.id[A].andThen(f)

  def categoryRightIdentity[A, B](f: F[A, B]): IsEq[F[A, B]] =
    f <-> f.andThen(F.id[B])
}

object CategoryLaws {
  def apply[F[_, _]] given Category[F]: CategoryLaws[F] =
    new CategoryLaws[F] with ComposeLaws[F]
}
