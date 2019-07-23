package io.circe.cats

import io.circe.cats.kernel.Semigroup
import io.circe.cats.syntax.TupleContravariantSemigroupalSyntax

/**
 * [[ContravariantSemigroupal]] is nothing more than something both contravariant
 * and Semigroupal. It comes up enough to be useful, and composes well
 */
trait ContravariantSemigroupal[F[_]] extends InvariantSemigroupal[F] with Contravariant[F] { self =>
  override def composeFunctor[G[_]] given Functor[G]: ContravariantSemigroupal[[x] =>> F[G[x]]] =
    new ComposedSemigroupal[F, G] {
      def F = self
      def G = the[Functor[G]]
    }

}

object ContravariantSemigroupal {
  def apply[F[_]] given (F: ContravariantSemigroupal[F]): ContravariantSemigroupal[F] = F

  def semigroup[F[_], A] given ContravariantSemigroupal[F]: Semigroup[F[A]] =
    new ContravariantSemigroupalSemigroup[F, A]

  given [F[_]] as ContravariantSemigroupal[F] given (F: ContravariantMonoidal[F]) = F

  private[cats] trait Ops extends TupleContravariantSemigroupalSyntax
}

private[cats] class ContravariantSemigroupalSemigroup[F[_], A] given (F: ContravariantSemigroupal[F]) extends Semigroup[F[A]] {
  def combine(a: F[A], b: F[A]): F[A] =
  	F.contramap(F.product(a, b))((a: A) => (a, a))
}
