package io.circe.cats.laws

import io.circe.cats.{Functor, FunctorFilter}
import io.circe.cats.kernel.laws.IsEq
import given io.circe.cats.syntax.functorFilter._

trait FunctorFilterLaws[F[_]] given (F: FunctorFilter[F]) {
  def mapFilterComposition[A, B, C](fa: F[A], f: A => Option[B], g: B => Option[C]): IsEq[F[C]] = {
    val lhs: F[C] = F.mapFilter(F.mapFilter(fa)(f))(g)
    val rhs: F[C] = F.mapFilter(fa)(a => f(a).flatMap(g))
    lhs <-> rhs
  }

  def mapFilterMapConsistency[A, B](fa: F[A], f: A => B): IsEq[F[B]] =
    F.mapFilter(fa)(f.andThen(x => Some(x): Option[B])) <-> F.functor.map(fa)(f)

  def collectConsistentWithMapFilter[A, B](fa: F[A], f: PartialFunction[A, B]): IsEq[F[B]] =
    F.collect(fa)(f) <-> F.mapFilter(fa)(f.lift)

  def flattenOptionConsistentWithMapFilter[A](fa: F[Option[A]]): IsEq[F[A]] =
    F.flattenOption(fa) <-> F.mapFilter(fa)(identity)

  def filterConsistentWithMapFilter[A](fa: F[A], f: A => Boolean): IsEq[F[A]] =
    F.filter(fa)(f) <->
      F.mapFilter(fa)(a => if (f(a)) Some(a) else None)
}

object FunctorFilterLaws {
  def apply[F[_]] given FunctorFilter[F]: FunctorFilterLaws[F] =
    new FunctorFilterLaws[F] {}
}
