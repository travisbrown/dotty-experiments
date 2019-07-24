package io.circe.cats.laws

import io.circe.cats.arrow.Arrow
import io.circe.cats.kernel.laws.IsEq
import given io.circe.cats.syntax.arrow._
import given io.circe.cats.syntax.compose._
import given io.circe.cats.syntax.strong._

/**
 * Laws that must be obeyed by any `cats.arrow.Arrow`.
 */
trait ArrowLaws[F[_, _]] given (F: Arrow[F]) extends CategoryLaws[F] with StrongLaws[F] {
  def arrowIdentity[A]: IsEq[F[A, A]] =
    F.lift(identity[A]) <-> F.id[A]

  def arrowComposition[A, B, C](f: A => B, g: B => C): IsEq[F[A, C]] =
    F.lift(f >>> g) <-> (F.lift(f) >>> F.lift(g))

  def arrowExtension[A, B, C](g: A => B): IsEq[F[(A, C), (B, C)]] =
    F.first[A, B, C](F.lift(g)) <-> F.lift(g.split(identity[C]))

  def arrowFunctor[A, B, C, D](f: F[A, B], g: F[B, C]): IsEq[F[(A, D), (C, D)]] =
    (f >>> g).first[D] <-> (f.first[D] >>> g.first[D])

  def arrowExchange[A, B, C, D](f: F[A, B], g: C => D): IsEq[F[(A, C), (B, D)]] =
    F.andThen(f.first[C], F.lift((identity[B]).split(g))) <-> (F.lift((identity[A] _).split(g)).andThen(f.first[D]))

  def arrowUnit[A, B, C](f: F[A, B]): IsEq[F[(A, C), B]] =
    F.andThen(f.first[C], F.lift(fst[B, C])) <-> (F.lift(fst[A, C]).andThen(f))

  def arrowAssociation[A, B, C, D](f: F[A, B]): IsEq[F[((A, C), D), (B, (C, D))]] =
    F.andThen(F.first[A, B, C](f).first[D], F.lift(assoc[B, C, D])) <-> (F.lift(assoc[A, C, D]).andThen(f.first[(C, D)]))

  def splitConsistentWithAndThen[A, B, C, D](f: F[A, B], g: F[C, D]): IsEq[F[(A, C), (B, D)]] =
    F.split(f, g) <-> (f.first.andThen(g.second))

  def mergeConsistentWithAndThen[A, B, C](f: F[A, B], g: F[A, C]): IsEq[F[A, (B, C)]] =
    F.merge(f, g) <-> ((F.lift((x: A) => (x, x))).andThen(F.split(f, g)))

  private def fst[A, B](p: (A, B)): A = p._1

  private def assoc[A, B, C](p: ((A, B), C)): (A, (B, C)) = (p._1._1, (p._1._2, p._2))
}

object ArrowLaws {
  def apply[F[_, _]] given Arrow[F]: ArrowLaws[F] =
    new ArrowLaws[F] with CategoryLaws[F] with StrongLaws[F] with ComposeLaws[F] with ProfunctorLaws[F]
}
