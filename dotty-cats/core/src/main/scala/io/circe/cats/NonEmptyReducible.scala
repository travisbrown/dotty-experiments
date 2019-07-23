package io.circe.cats

import io.circe.cats.data.NonEmptyList
import io.circe.cats.kernel.Monoid

/**
 * This class defines a `Reducible[F]` in terms of a `Foldable[G]`
 * together with a `split` method, `F[A]` => `(A, G[A])`.
 *
 * This class can be used on any type where the first value (`A`) and
 * the "rest" of the values (`G[A]`) can be easily found.
 */
abstract class NonEmptyReducible[F[_], G[_]] given (G: Foldable[G]) extends Reducible[F] {
  def split[A](fa: F[A]): (A, G[A])

  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B = {
    val (a, ga) = split(fa)
    G.foldLeft(ga, f(b, a))(f)
  }

  def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    Always(split(fa)).flatMap {
      case (a, ga) =>
        f(a, G.foldRight(ga, lb)(f))
    }

  def reduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): B = {
    val (a, ga) = split(fa)
    G.foldLeft(ga, f(a))((b, a) => g(b, a))
  }

  def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
    Always(split(fa)).flatMap {
      case (a, ga) =>
        G.reduceRightToOption(ga)(f)(g).flatMap {
          case Some(b) => g(a, Now(b))
          case None    => Later(f(a))
        }
    }

  override def size[A](fa: F[A]): Long = {
    val (_, tail) = split(fa)
    1 + G.size(tail)
  }

  override def get[A](fa: F[A])(idx: Long): Option[A] =
    if (idx == 0L) Some(split(fa)._1) else G.get(split(fa)._2)(idx - 1L)

  override def fold[A](fa: F[A]) given (A: Monoid[A]): A = {
    val (a, ga) = split(fa)
    A.combine(a, G.fold(ga))
  }

  override def foldM[H[_], A, B](fa: F[A], z: B)(f: (B, A) => H[B]) given (H: Monad[H]): H[B] = {
    val (a, ga) = split(fa)
    H.flatMap(f(z, a))(G.foldM(ga, _)(f))
  }

  override def find[A](fa: F[A])(f: A => Boolean): Option[A] = {
    val (a, ga) = split(fa)
    if (f(a)) Some(a) else G.find(ga)(f)
  }

  override def exists[A](fa: F[A])(p: A => Boolean): Boolean = {
    val (a, ga) = split(fa)
    p(a) || G.exists(ga)(p)
  }

  override def forall[A](fa: F[A])(p: A => Boolean): Boolean = {
    val (a, ga) = split(fa)
    p(a) && G.forall(ga)(p)
  }

  override def toList[A](fa: F[A]): List[A] = {
    val (a, ga) = split(fa)
    a :: G.toList(ga)
  }

  override def toNonEmptyList[A](fa: F[A]): NonEmptyList[A] = {
    val (a, ga) = split(fa)
    NonEmptyList(a, G.toList(ga))
  }

  override def filter_[A](fa: F[A])(p: A => Boolean): List[A] = {
    val (a, ga) = split(fa)
    val filteredTail = G.filter_(ga)(p)
    if (p(a)) a :: filteredTail else filteredTail
  }

  override def takeWhile_[A](fa: F[A])(p: A => Boolean): List[A] = {
    val (a, ga) = split(fa)
    if (p(a)) a :: G.takeWhile_(ga)(p) else Nil
  }

  override def dropWhile_[A](fa: F[A])(p: A => Boolean): List[A] = {
    val (a, ga) = split(fa)
    if (p(a)) G.dropWhile_(ga)(p) else a :: G.toList(ga)
  }
}

object NonEmptyReducible {
  def apply[F[_], G[_]] given (F: NonEmptyReducible[F, G]): NonEmptyReducible[F, G] = F
}
