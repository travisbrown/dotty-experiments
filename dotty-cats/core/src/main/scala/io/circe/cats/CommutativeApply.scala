package io.circe.cats

import io.circe.cats.kernel.CommutativeSemigroup

/**
 * Commutative Apply.
 *
 * Further than an Apply, which just allows composition of independent effectful functions,
 * in a Commutative Apply those functions can be composed in any order, which guarantees
 * that their effects do not interfere.
 *
 * Must obey the laws defined in cats.laws.CommutativeApplyLaws.
 */
trait CommutativeApply[F[_]] extends Apply[F]

object CommutativeApply {
  def apply[F[_]] given (F: CommutativeApply[F]): CommutativeApply[F] = F

  def commutativeSemigroupFor[F[_], A] given (F: CommutativeApply[F], A: CommutativeSemigroup[A]): CommutativeSemigroup[F[A]] =
    new CommutativeSemigroup[F[A]] {
      def combine(x: F[A], y: F[A]): F[A] =
        F.map2(x, y)(A.combine)
    }
}
