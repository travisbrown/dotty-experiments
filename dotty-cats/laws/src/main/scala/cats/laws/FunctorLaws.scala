package io.circe.cats.laws

import io.circe.cats.Functor
import io.circe.cats.kernel.laws.IsEq
import given io.circe.cats.syntax.functor._

/**
 * Laws that must be obeyed by any `Functor`.
 */
trait FunctorLaws[F[_]] given Functor[F] extends InvariantLaws[F] {
  def covariantIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa.map(identity) <-> fa

  def covariantComposition[A, B, C](fa: F[A], f: A => B, g: B => C): IsEq[F[C]] =
    fa.map(f).map(g) <-> fa.map(f.andThen(g))
}

object FunctorLaws {
  def apply[F[_]] given Functor[F]: FunctorLaws[F] =
    new FunctorLaws[F] with InvariantLaws[F]
}
