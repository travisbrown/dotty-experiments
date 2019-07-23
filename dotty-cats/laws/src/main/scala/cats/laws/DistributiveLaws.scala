package io.circe.cats.laws

import io.circe.cats.{Distributive, Functor, Id}
import io.circe.cats.data.Nested
import io.circe.cats.kernel.laws.IsEq
import given io.circe.cats.syntax.distributive._

trait DistributiveLaws[F[_]] given (F: Distributive[F]) extends FunctorLaws[F] {
  def distributeIdentity[A, B](fa: F[A], f: A => B): IsEq[F[B]] =
    the[Distributive[Id]].distribute[F, A, B](fa)(f) <-> F.map(fa)(f)

  def cosequenceIdentity[A](fa: F[A]): IsEq[F[A]] =
    F.cosequence[Id, A](fa) <-> fa

  def cosequenceTwiceIsId[A, M[_]](fma: F[M[A]]) given (M: Distributive[M]): IsEq[F[M[A]]] = {
    val result = F.cosequence(M.cosequence(fma))
    fma <-> result
  }

  def composition[A, B, C, M[_], N[_]](
    ma: M[A],
    f: A => F[B],
    g: B => N[C]
  ) given Distributive[N], Functor[M]: IsEq[Nested[F, N, M[C]]] = {
    val rhs = ma.distribute[[x] =>> Nested[F, N, x], C](a => Nested(F.map(f(a))(g)))
    val lhs = Nested(F.map(ma.distribute(f))(fb => fb.distribute(g)))
    lhs <-> rhs
  }
}

object DistributiveLaws {
  def apply[F[_]] given Distributive[F]: DistributiveLaws[F] =
    new DistributiveLaws[F] with FunctorLaws[F] with InvariantLaws[F]
}
