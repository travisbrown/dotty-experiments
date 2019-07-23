package io.circe.cats.laws

import io.circe.cats.CommutativeFlatMap
import io.circe.cats.kernel.laws.IsEq

/**
 * Laws that must be obeyed by any `CommutativeFlatMap`.
 */
trait CommutativeFlatMapLaws[F[_]] given (F: CommutativeFlatMap[F]) extends CommutativeApplyLaws[F] with FlatMapLaws[F] {
  def flatmapCommutative[A, B, C](fa: F[A], fb: F[B], g: (A, B) => F[C]): IsEq[F[C]] =
    F.flatMap(fa)(a => F.flatMap(fb)(b => g(a, b))) <->
      F.flatMap(fb)(b => F.flatMap(fa)(a => g(a, b)))
}

object CommutativeFlatMapLaws {
  def apply[F[_]] given CommutativeFlatMap[F]: CommutativeFlatMapLaws[F] =
    new CommutativeFlatMapLaws[F] with CommutativeApplyLaws[F] with FlatMapLaws[F] with ApplyLaws[F] with SemigroupalLaws[F] with FunctorLaws[F] with InvariantLaws[F]
}
