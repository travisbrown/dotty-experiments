package io.circe.cats.laws

import io.circe.cats.NonEmptyParallel
import io.circe.cats.kernel.laws.IsEq

/**
 * Laws that must be obeyed by any `cats.NonEmptyParallel`.
 */
trait NonEmptyParallelLaws[M[_], F[_]] given (M: NonEmptyParallel[M, F]) {
  def parallelRoundTrip[A](ma: M[A]): IsEq[M[A]] =
    M.sequential(M.parallel(ma)) <-> ma

  def sequentialRoundTrip[A](fa: F[A]): IsEq[F[A]] =
    M.parallel(M.sequential(fa)) <-> fa

  def isomorphicFunctor[A, B](fa: F[A], f: A => B): IsEq[M[B]] =
    M.flatMap.map(M.sequential(fa))(f) <-> M.sequential(M.apply.map(fa)(f))
}

object NonEmptyParallelLaws {
  def apply[M[_], F[_]] given NonEmptyParallel[M, F]: NonEmptyParallelLaws[M, F] =
    new NonEmptyParallelLaws[M, F] {}
}
