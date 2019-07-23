package io.circe.cats.laws

import io.circe.cats.CommutativeMonad
import io.circe.cats.kernel.laws.IsEq

/**
 * Laws that must be obeyed by any `CommutativeMonad`.
 */
trait CommutativeMonadLaws[F[_]] given CommutativeMonad[F]
    extends MonadLaws[F]
    with CommutativeFlatMapLaws[F]
    with CommutativeApplicativeLaws[F]

object CommutativeMonadLaws {
  def apply[F[_]] given CommutativeMonad[F]: CommutativeMonadLaws[F] =
    new CommutativeMonadLaws[F] with MonadLaws[F] with CommutativeFlatMapLaws[F] with CommutativeApplicativeLaws[F]
      with CommutativeApplyLaws[F]
      with FlatMapLaws[F] with ApplicativeLaws[F] with ApplyLaws[F] with SemigroupalLaws[F] with FunctorLaws[F] with InvariantLaws[F]
}
