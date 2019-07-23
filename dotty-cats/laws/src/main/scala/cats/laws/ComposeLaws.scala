package io.circe.cats.laws

import io.circe.cats.arrow.Compose
import io.circe.cats.kernel.laws.IsEq
import given io.circe.cats.syntax.compose._

/**
 * Laws that must be obeyed by any `cats.arrow.Compose`.
 */
trait ComposeLaws[F[_, _]] given (F: Compose[F]) {
  def composeAssociativity[A, B, C, D](fab: F[A, B], fbc: F[B, C], fcd: F[C, D]): IsEq[F[A, D]] =
    F.andThen(fab.andThen(fbc), fcd) <-> fab.andThen(fbc.andThen(fcd))
}

object ComposeLaws {
  def apply[F[_, _]] given Compose[F]: ComposeLaws[F] =
    new ComposeLaws[F] {}
}
