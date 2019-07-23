package io.circe.cats.laws

import io.circe.cats.Parallel
import io.circe.cats.kernel.laws.IsEq

/**
 * Laws that must be obeyed by any `cats.Parallel`.
 */
trait ParallelLaws[M[_], F[_]] given (M: Parallel[M, F]) extends NonEmptyParallelLaws[M, F] {
  def isomorphicPure[A](a: A): IsEq[F[A]] =
    M.applicative.pure(a) <-> M.parallel(M.monad.pure(a))
}

object ParallelLaws {
  def apply[M[_], F[_]] given Parallel[M, F]: ParallelLaws[M, F] =
    new ParallelLaws[M, F] with NonEmptyParallelLaws[M, F]
}
