package io.circe.cats.laws

import io.circe.cats.CommutativeApply
import io.circe.cats.kernel.laws.IsEq

/**
 * Laws that must be obeyed by any `CommutativeApply`.
 */
trait CommutativeApplyLaws[F[_]] given (F: CommutativeApply[F]) extends ApplyLaws[F] {
  def applyCommutative[A, B, C](fa: F[A], fb: F[B], f: (A, B) => C): IsEq[F[C]] =
    F.map2(fa, fb)(f) <-> F.map2(fb, fa)((b, a) => f(a, b))
}

object CommutativeApplyLaws {
  def apply[F[_]] given CommutativeApply[F]: CommutativeApplyLaws[F] =
    new CommutativeApplyLaws[F] with ApplyLaws[F] with SemigroupalLaws[F] with FunctorLaws[F] with InvariantLaws[F]
}
