package io.circe.cats.laws

import io.circe.cats.{Applicative, TraverseFilter}
import io.circe.cats.data.Nested
import io.circe.cats.kernel.laws.IsEq
import given io.circe.cats.syntax.functor._
import given io.circe.cats.syntax.traverseFilter._

trait TraverseFilterLaws[F[_]] given (F: TraverseFilter[F]) extends FunctorFilterLaws[F] {
  def traverseFilterIdentity[G[_], A](fa: F[A]) given (G: Applicative[G]): IsEq[G[F[A]]] =
    fa.traverseFilter(a => G.pure(Some(a))) <-> G.pure(fa)

  def traverseFilterConsistentWithTraverse[G[_], A](fa: F[A], f: A => G[A]) given Applicative[G]: IsEq[G[F[A]]] =
    fa.traverseFilter(a => f(a).map(Some(_))) <-> F.traverse.traverse(fa)(f)

  def traverseFilterComposition[A, B, C, M[_], N[_]](fa: F[A], f: A => M[Option[B]], g: B => N[Option[C]]) given (
    M: Applicative[M],
    N: Applicative[N]
  ): IsEq[Nested[M, N, F[C]]] = {    val lhs = Nested[M, N, F[C]](fa.traverseFilter(f).map(_.traverseFilter(g)))
    val rhs: Nested[M, N, F[C]] =
      fa.traverseFilter[[x] =>> Nested[M, N, x], C](a => Nested[M, N, Option[C]](f(a).map(_.traverseFilter(g))))
    lhs <-> rhs
  }

  def filterAConsistentWithTraverseFilter[G[_], A](fa: F[A], f: A => G[Boolean]) given Applicative[G]: IsEq[G[F[A]]] =
    fa.filterA(f) <-> fa.traverseFilter(a => f(a).map(if (_) Some(a) else None))
}

object TraverseFilterLaws {
  def apply[F[_]] given TraverseFilter[F]: TraverseFilterLaws[F] =
    new TraverseFilterLaws[F] with FunctorFilterLaws[F]
}
