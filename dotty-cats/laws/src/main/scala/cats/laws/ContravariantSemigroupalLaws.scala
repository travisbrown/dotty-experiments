package io.circe.cats.laws

import io.circe.cats.ContravariantSemigroupal
import io.circe.cats.kernel.laws.IsEq
import given io.circe.cats.syntax.contravariantSemigroupal._

/**
 * Laws that are expected for any `cats.ContravariantSemigroupal`.
 */
trait ContravariantSemigroupalLaws[F[_]] given ContravariantSemigroupal[F] extends ContravariantLaws[F] with SemigroupalLaws[F] {
  def delta[A](a: A): (A, A) = (a, a)

  def contravariantSemigroupalContramap2DiagonalAssociates[A](m: F[A], n: F[A], o: F[A]): IsEq[F[A]] =
    ((m, n).contramapN(delta[A]), o).contramapN(delta[A]) <-> (m, (n, o).contramapN(delta[A])).contramapN(delta[A])
}

object ContravariantSemigroupalLaws {
  def apply[F[_]] given ContravariantSemigroupal[F]: ContravariantSemigroupalLaws[F] =
    new ContravariantSemigroupalLaws[F] with ContravariantLaws[F] with SemigroupalLaws[F] with InvariantLaws[F]
}
