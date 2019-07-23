package io.circe.cats.instances

import io.circe.cats.{CommutativeApplicative, MonoidK, UnorderedTraverse}
import io.circe.cats.kernel.CommutativeMonoid

private[cats] object SetInstance extends UnorderedTraverse[Set] with MonoidK[Set] {
  def unorderedTraverse[G[_], A, B](sa: Set[A])(f: A => G[B]) given (G: CommutativeApplicative[G]): G[Set[B]] =
    sa.foldLeft(G.pure(Set.empty[B])) { (acc, a) =>
      G.map2(acc, f(a))(_ + _)
    }

  override def unorderedSequence[G[_], A](sa: Set[G[A]]) given (G: CommutativeApplicative[G]): G[Set[A]] =
    sa.foldLeft(G.pure(Set.empty[A])) { (acc, a) =>
      G.map2(acc, a)(_ + _)
    }

  def empty[A]: Set[A] = Set.empty[A]

  def combineK[A](x: Set[A], y: Set[A]): Set[A] = x | y

  def unorderedFoldMap[A, B](fa: Set[A])(f: A => B) given (B: CommutativeMonoid[B]): B =
    fa.foldLeft(B.empty)((b, a) => B.combine(f(a), b))

  override def unorderedFold[A](fa: Set[A]) given (A: CommutativeMonoid[A]): A = A.combineAll(fa)

  override def forall[A](fa: Set[A])(p: A => Boolean): Boolean =
    fa.forall(p)

  override def exists[A](fa: Set[A])(p: A => Boolean): Boolean =
    fa.exists(p)

  override def isEmpty[A](fa: Set[A]): Boolean = fa.isEmpty
}
