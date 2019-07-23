package io.circe.cats.laws

import io.circe.cats.CoflatMap
import io.circe.cats.data.Cokleisli
import io.circe.cats.kernel.laws.IsEq
import given io.circe.cats.syntax.coflatMap._
import given io.circe.cats.syntax.functor._

/**
 * Laws that must be obeyed by any `CoflatMap`.
 */
trait CoflatMapLaws[F[_]] given (F: CoflatMap[F]) extends FunctorLaws[F] {
  def coflatMapAssociativity[A, B, C](fa: F[A], f: F[A] => B, g: F[B] => C): IsEq[F[C]] =
    F.coflatMap(fa.coflatMap(f))(g) <-> fa.coflatMap(x => g(x.coflatMap(f)))

  def coflattenThroughMap[A](fa: F[A]): IsEq[F[F[F[A]]]] =
    F.coflatten(fa.coflatten) <-> fa.coflatten.map(_.coflatten)

  def coflattenCoherence[A, B](fa: F[A], f: F[A] => B): IsEq[F[B]] =
    F.coflatMap(fa)(f) <-> fa.coflatten.map(f)

  def coflatMapIdentity[A, B](fa: F[A]): IsEq[F[F[A]]] =
    F.coflatten(fa) <-> fa.coflatMap(identity)

  /**
   * The composition of `cats.data.Cokleisli` arrows is associative. This is
   * analogous to [[coflatMapAssociativity]].
   */
  def cokleisliAssociativity[A, B, C, D](f: F[A] => B, g: F[B] => C, h: F[C] => D, fa: F[A]): IsEq[D] = {
    val (cf, cg, ch) = (Cokleisli(f), Cokleisli(g), Cokleisli(h))
    (cf.andThen(cg)).andThen(ch).run(fa) <-> cf.andThen(cg.andThen(ch)).run(fa)
  }
}

object CoflatMapLaws {
  def apply[F[_]] given CoflatMap[F]: CoflatMapLaws[F] =
    new CoflatMapLaws[F] with FunctorLaws[F] with InvariantLaws[F]
}
