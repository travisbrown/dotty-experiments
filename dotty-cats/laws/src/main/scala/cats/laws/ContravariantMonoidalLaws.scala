package io.circe.cats.laws

import io.circe.cats.ContravariantMonoidal
import io.circe.cats.kernel.laws.IsEq
import given io.circe.cats.syntax.contravariant._
import given io.circe.cats.syntax.contravariantSemigroupal._

/**
 * Laws that must hold for any `cats.ContravariantMonoidal`.
 */
trait ContravariantMonoidalLaws[F[_]] given (F: ContravariantMonoidal[F]) extends ContravariantSemigroupalLaws[F] {
  def contravariantMonoidalUnitRight[A](fa: F[A]): IsEq[F[A]] =
    (fa, F.trivial[A]).contramapN(delta[A]) <-> fa

  def contravariantMonoidalUnitLeft[A](fa: F[A]): IsEq[F[A]] =
    (F.trivial[A], fa).contramapN(delta[A]) <-> fa

  def contravariantMonoidalContramap2CompatibleContramapLeft[A, B, C](fa: F[A], f: B => (A, C)): IsEq[F[B]] =
    (fa, F.trivial[C]).contramapN(f) <-> fa.contramap(f.andThen(_._1))

  def contravariantMonoidalContramap2CompatibleContramapRight[A, B, C](fa: F[A], f: C => (B, A)): IsEq[F[C]] =
    (F.trivial[B], fa).contramapN(f) <-> fa.contramap(f.andThen(_._2))
}

object ContravariantMonoidalLaws {
  def apply[F[_]] given ContravariantMonoidal[F]: ContravariantMonoidalLaws[F] =
    new ContravariantMonoidalLaws[F] with ContravariantSemigroupalLaws[F] with SemigroupalLaws[F] with ContravariantLaws[F] with InvariantLaws[F]
}
