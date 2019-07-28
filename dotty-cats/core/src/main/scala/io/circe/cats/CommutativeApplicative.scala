package io.circe.cats

import io.circe.cats.kernel.CommutativeMonoid

/**
 * Commutative Applicative.
 *
 * Further than an Applicative, which just allows composition of independent effectful functions,
 * in a Commutative Applicative those functions can be composed in any order, which guarantees
 * that their effects do not interfere.
 *
 * Must obey the laws defined in cats.laws.CommutativeApplicativeLaws.
 */
trait CommutativeApplicative[F[_]] extends Applicative[F] with CommutativeApply[F]

object CommutativeApplicative {
  def apply[F[_]] given (F: CommutativeApplicative[F]): CommutativeApplicative[F] = F

  def commutativeMonoidFor[F[_], A] given (F: CommutativeApplicative[F], A: CommutativeMonoid[A]): CommutativeMonoid[F[A]] =
    new CommutativeMonoid[F[A]] {
      override def empty: F[A] =
        F.pure(A.empty)

      override def combine(x: F[A], y: F[A]): F[A] =
        F.map2(x, y)(A.combine)
    }
}
