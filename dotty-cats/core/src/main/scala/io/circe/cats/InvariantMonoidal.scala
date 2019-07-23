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

  given as InvariantMonoidal[Semigroup] {
    def product[A, B](fa: Semigroup[A], fb: Semigroup[B]): Semigroup[(A, B)] = new Semigroup[(A, B)] {
      def combine(x: (A, B), y: (A, B)): (A, B) = fa.combine(x._1, y._1) -> fb.combine(x._2, y._2)
    }

    def imap[A, B](fa: Semigroup[A])(f: A => B)(g: B => A): Semigroup[B] = new Semigroup[B] {
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
    }

    def unit: Semigroup[Unit] = the[Semigroup[Unit]]
  }

  given as InvariantMonoidal[CommutativeSemigroup] {
    def product[A, B](fa: CommutativeSemigroup[A], fb: CommutativeSemigroup[B]): CommutativeSemigroup[(A, B)] =
      new CommutativeSemigroup[(A, B)] {
        def combine(x: (A, B), y: (A, B)): (A, B) = fa.combine(x._1, y._1) -> fb.combine(x._2, y._2)
      }

    def imap[A, B](fa: CommutativeSemigroup[A])(f: A => B)(g: B => A): CommutativeSemigroup[B] =
      new CommutativeSemigroup[B] {
        def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      }

    def unit: CommutativeSemigroup[Unit] = the[CommutativeSemigroup[Unit]]
  }
}