package io.circe.cats.laws

import io.circe.cats.Monad
import io.circe.cats.data.Kleisli
import io.circe.cats.kernel.laws.IsEq
import given io.circe.cats.syntax.flatMap._
import given io.circe.cats.syntax.functor._

/**
 * Laws that must be obeyed by any `Monad`.
 */
trait MonadLaws[F[_]] given (F: Monad[F]) extends ApplicativeLaws[F] with FlatMapLaws[F] {
  def monadLeftIdentity[A, B](a: A, f: A => F[B]): IsEq[F[B]] =
    F.pure(a).flatMap(f) <-> f(a)

  def monadRightIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa.flatMap(F.pure) <-> fa

  /**
   * `pure` is the left identity element under left-to-right composition of
   * `cats.data.Kleisli` arrows. This is analogous to [[monadLeftIdentity]].
   */
  def kleisliLeftIdentity[A, B](a: A, f: A => F[B]): IsEq[F[B]] =
    Kleisli(F.pure[A]).andThen(Kleisli(f)).run(a) <-> f(a)

  /**
   * `pure` is the right identity element under left-to-right composition of
   * `cats.data.Kleisli` arrows. This is analogous to [[monadRightIdentity]].
   */
  def kleisliRightIdentity[A, B](a: A, f: A => F[B]): IsEq[F[B]] =
    Kleisli(f).andThen(Kleisli(F.pure[B])).run(a) <-> f(a)

  /**
   * Make sure that map and flatMap are consistent.
   */
  def mapFlatMapCoherence[A, B](fa: F[A], f: A => B): IsEq[F[B]] =
    fa.flatMap(a => F.pure(f(a))) <-> fa.map(f)

  lazy val tailRecMStackSafety: IsEq[F[Int]] = {
    val n = 50000
    val res = F.tailRecM(0)(i => F.pure(if (i < n) Left(i + 1) else Right(i)))
    res <-> F.pure(n)
  }
}

object MonadLaws {
  def apply[F[_]] given Monad[F]: MonadLaws[F] =
    new MonadLaws[F] with ApplicativeLaws[F] with FlatMapLaws[F] with ApplyLaws[F] with SemigroupalLaws[F] with FunctorLaws[F] with InvariantLaws[F]
}
