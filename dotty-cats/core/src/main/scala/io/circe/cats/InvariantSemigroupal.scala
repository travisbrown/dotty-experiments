package io.circe.cats

import io.circe.cats.kernel.{Monoid, Semigroup}

/**
 * [[InvariantSemigroupal]] is nothing more than something both invariant
 * and Semigroupal. It comes up enough to be useful, and composes well
 */
trait InvariantSemigroupal[F[_]] extends Semigroupal[F] with Invariant[F] { self =>

  def composeApply[G[_]] given Apply[G]: InvariantSemigroupal[[x] =>> F[G[x]]] =
    new ComposedInvariantApplySemigroupal[F, G] {
      def F = self
      def G = the[Apply[G]]
    }

}

object InvariantSemigroupal {
  def apply[F[_]] given (F: InvariantSemigroupal[F]): InvariantSemigroupal[F] = F

  /**
   * Gives a `Semigroup` instance if A itself has a `Semigroup` instance.
   */
  def semigroup[F[_], A] given InvariantSemigroupal[F], Semigroup[A]: Semigroup[F[A]] =
    new InvariantSemigroupalSemigroup[F, A]

  given as InvariantSemigroupal[Monoid] {
    def product[A, B](fa: Monoid[A], fb: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
      val empty = fa.empty -> fb.empty
      def combine(x: (A, B), y: (A, B)): (A, B) = fa.combine(x._1, y._1) -> fb.combine(x._2, y._2)
    }

    def imap[A, B](fa: Monoid[A])(f: A => B)(g: B => A): Monoid[B] = new Monoid[B] {
      def empty: B = f(fa.empty)

      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
    }
  }
}

private[cats] class InvariantSemigroupalSemigroup[F[_], A] given (F: InvariantSemigroupal[F], A: Semigroup[A])
    extends Semigroup[F[A]] {
  def combine(a: F[A], b: F[A]): F[A] =
    F.imap(F.product(a, b))(A.combine)(a => (a, a))
}
