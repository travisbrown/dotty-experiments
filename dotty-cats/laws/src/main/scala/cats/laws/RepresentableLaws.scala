package io.circe.cats.laws

import io.circe.cats.Representable
import io.circe.cats.kernel.laws.IsEq

/**
 * Laws that must be obeyed by any `Representable` functor.
 */
trait RepresentableLaws[F[_], R] given (R: Representable.Aux[F, R]) {
  def indexTabulateIsId[B](fb: F[B]): IsEq[F[B]] =
    R.tabulate(R.index(fb)) <-> fb

  def tabulateIndexIsId[B](f: R => B, x: R): IsEq[B] =
    R.index(R.tabulate(f))(x) <-> f(x)
}

object RepresentableLaws {
  def apply[F[_], R] given Representable.Aux[F, R]: RepresentableLaws[F, R] =
    new RepresentableLaws[F, R] {}
}
