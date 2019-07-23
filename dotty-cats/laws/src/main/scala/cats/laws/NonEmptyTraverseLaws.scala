package io.circe.cats.laws

import io.circe.cats.{ Apply, Id, Monad, NonEmptyTraverse }
import io.circe.cats.data.{ Const, Nested }
import io.circe.cats.kernel.Semigroup
import io.circe.cats.kernel.laws.IsEq
import given io.circe.cats.syntax.nonEmptyTraverse._
import given io.circe.cats.syntax.reducible._

trait NonEmptyTraverseLaws[F[_]] given (F: NonEmptyTraverse[F]) extends TraverseLaws[F] with ReducibleLaws[F] {
  def nonEmptyTraverseIdentity[A, B](fa: F[A], f: A => B): IsEq[F[B]] =
    fa.nonEmptyTraverse[Id, B](f) <-> F.map(fa)(f)

  def nonEmptyTraverseSequentialComposition[A, B, C, M[_], N[_]](
    fa: F[A],
    f: A => M[B],
    g: B => N[C]
  ) given (
    M: Apply[M],
    N: Apply[N]): IsEq[Nested[M, N, F[C]]] = {

    val lhs = Nested(M.map(fa.nonEmptyTraverse(f))(fb => fb.nonEmptyTraverse(g)))
    val rhs = fa.nonEmptyTraverse[[x] =>> Nested[M, N, x], C](a => Nested(M.map(f(a))(g)))
    lhs <-> rhs
  }

  def nonEmptyTraverseParallelComposition[A, B, M[_], N[_]](
    fa: F[A],
    f: A => M[B],
    g: A => N[B]
  ) given (
    M: Apply[M],
    N: Apply[N]): IsEq[(M[F[B]], N[F[B]])] = {
    type MN[Z] = (M[Z], N[Z])

    given as Apply[MN] {
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
    val lhs: MN[F[B]] = fa.nonEmptyTraverse[MN, B](a => (f(a), g(a)))
    val rhs: MN[F[B]] = (fa.nonEmptyTraverse(f), fa.nonEmptyTraverse(g))
    lhs <-> rhs
  }

  def reduceMapDerived[A, B](
    fa: F[A],
    f: A => B
  ) given (B: Semigroup[B]): IsEq[B] = {
    val lhs: B = fa.nonEmptyTraverse[[x] =>> Const[B, x], B](a => Const(f(a))).getConst
    val rhs: B = fa.reduceMap(f)
    lhs <-> rhs
  }
}

object NonEmptyTraverseLaws {
  def apply[F[_]] given NonEmptyTraverse[F]: NonEmptyTraverseLaws[F] =
    new NonEmptyTraverseLaws[F] with TraverseLaws[F] with ReducibleLaws[F] with UnorderedTraverseLaws[F] with FoldableLaws[F] with UnorderedFoldableLaws[F] with FunctorLaws[F] with InvariantLaws[F]
}
