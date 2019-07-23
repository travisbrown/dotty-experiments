package io.circe.cats.laws

import io.circe.cats.{ CommutativeApplicative, Functor, Id, UnorderedTraverse }
import io.circe.cats.data.Nested
import io.circe.cats.kernel.laws.IsEq
import io.circe.cats.UnorderedFoldable

trait UnorderedTraverseLaws[F[_]] given (F: UnorderedTraverse[F]) extends UnorderedFoldableLaws[F] {
  def unorderedTraverseIdentity[A, B](fa: F[A])(f: A => B) given Functor[F]: IsEq[F[B]] =
    F.unorderedTraverse[Id, A, B](fa)(f) <-> (the[Functor[F]].map(fa)(f))

  def unorderedTraverseSequentialComposition[A, B, C, M[_], N[_]](fa: F[A], f: A => M[B], g: B => N[C]) given (
    M: CommutativeApplicative[M],
    N: CommutativeApplicative[N]
  ): IsEq[Nested[M, N, F[C]]] = {

    val lhs = Nested(M.map(F.unorderedTraverse(fa)(f))(fb => F.unorderedTraverse(fb)(g)))
    val rhs = F.unorderedTraverse[[x] =>> Nested[M, N, x], A, C](fa)(a => Nested(M.map(f(a))(g)))
    lhs <-> rhs
  }

  def unorderedTraverseParallelComposition[A, B, M[_], N[_]](fa: F[A], f: A => M[B], g: A => N[B]) given (
    M: CommutativeApplicative[M],
    N: CommutativeApplicative[N],
  ): IsEq[(M[F[B]], N[F[B]])] = {

    type MN[Z] = (M[Z], N[Z])

    given MN as CommutativeApplicative[MN] {
      def pure[X](x: X): MN[X] = (M.pure(x), N.pure(x))
      def ap[X, Y](f: MN[X => Y])(fa: MN[X]): MN[Y] = {
        val (fam, fan) = fa
        val (fm, fn) = f
        (M.ap(fm)(fam), N.ap(fn)(fan))
      }
      override def map[X, Y](fx: MN[X])(f: X => Y): MN[Y] = {
        val (mx, nx) = fx
        (M.map(mx)(f), N.map(nx)(f))
      }
      override def product[X, Y](fx: MN[X], fy: MN[Y]): MN[(X, Y)] = {
        val (mx, nx) = fx
        val (my, ny) = fy
        (M.product(mx, my), N.product(nx, ny))
      }
    }
    val lhs: MN[F[B]] = F.unorderedTraverse[MN, A, B](fa)(a => (f(a), g(a)))
    val rhs: MN[F[B]] = (F.unorderedTraverse(fa)(f), F.unorderedTraverse(fa)(g))
    lhs <-> rhs
  }

  def unorderedSequenceConsistent[A, G[_]](fga: F[G[A]]) given CommutativeApplicative[G]: IsEq[G[F[A]]] =
    F.unorderedTraverse(fga)(identity) <-> F.unorderedSequence(fga)
}

object UnorderedTraverseLaws {
  def apply[F[_]] given UnorderedTraverse[F]: UnorderedTraverseLaws[F] =
    new UnorderedTraverseLaws[F] with UnorderedFoldableLaws[F]
}
