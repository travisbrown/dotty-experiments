package io.circe.cats

import io.circe.cats.kernel.{CommutativeSemigroup, Monoid, Semigroup}

/**
 * Invariant version of a Monoidal.
 *
 * Must obey the laws defined in cats.laws.InvariantMonoidalLaws.
 */
trait InvariantMonoidal[F[_]] extends InvariantSemigroupal[F] {

  /**
   * `point` lifts any value into a Monoidal Functor.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> InvariantMonoidal[Option].point(10)
   * res0: Option[Int] = Some(10)
   * }}}
   */
  def point[A](a: A): F[A] = imap(unit)(_ => a)(_ => ())

  def unit: F[Unit]
}

object InvariantMonoidal {
  def apply[F[_]] given (F: InvariantMonoidal[F]): InvariantMonoidal[F] = F

  /**
   * Gives a `Monoid` instance if A itself has a `Monoid` instance.
   */
  def monoid[F[_], A] given (F: InvariantMonoidal[F], A: Monoid[A]): Monoid[F[A]] =
    new InvariantSemigroupalSemigroup[F, A] with Monoid[F[A]] {
      def empty: F[A] = F.point(A.empty)
    }
}