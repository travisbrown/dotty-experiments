package io.circe.cats.laws

import io.circe.cats.{ Applicative, Now, Reducible }
import io.circe.cats.kernel.Semigroup
import io.circe.cats.kernel.laws.IsEq
import given io.circe.cats.syntax.foldable._
import given io.circe.cats.syntax.semigroup._
import given io.circe.cats.syntax.reducible._
import given io.circe.cats.syntax.unorderedFoldable._

trait ReducibleLaws[F[_]] given (F: Reducible[F]) extends FoldableLaws[F] {
  def reduceLeftToConsistentWithReduceMap[A, B](
    fa: F[A],
    f: A => B
  ) given Semigroup[B]: IsEq[B] =
    fa.reduceMap(f) <-> fa.reduceLeftTo(f)((b, a) => b |+| f(a))

  def reduceRightToConsistentWithReduceMap[A, B](
    fa: F[A],
    f: A => B
  ) given Semigroup[B]: IsEq[B] =
    fa.reduceMap(f) <-> fa.reduceRightTo(f)((a, eb) => eb.map(f(a) |+| _)).value

  def reduceRightToConsistentWithReduceRightToOption[A, B](
    fa: F[A],
    f: A => B
  ) given Semigroup[B]: IsEq[Option[B]] =
    fa.reduceRightToOption(f)((a, eb) => eb.map(f(a) |+| _)).value <->
      fa.reduceRightTo(f)((a, eb) => eb.map(f(a) |+| _)).map(Option(_)).value

  def reduceRightConsistentWithReduceRightOption[A](fa: F[A], f: (A, A) => A): IsEq[Option[A]] =
    fa.reduceRight((a1, e2) => Now(f(a1, e2.value))).map(Option(_)).value <->
      fa.reduceRightOption((a1, e2) => Now(f(a1, e2.value))).value

  def reduceReduceLeftConsistent[B](fa: F[B]) given (B: Semigroup[B]): IsEq[B] =
    F.reduce(fa) <-> fa.reduceLeft(B.combine)

  def traverseConsistent[G[_], A, B](fa: F[A], f: A => G[B]) given Applicative[G]: IsEq[G[Unit]] =
    fa.nonEmptyTraverse_(f) <-> fa.traverse_(f)

  def sequenceConsistent[G[_], A](fa: F[G[A]]) given Applicative[G]: IsEq[G[Unit]] =
    F.nonEmptySequence_(fa) <-> fa.sequence_

  def sizeConsistent[A](fa: F[A]): IsEq[Long] =
    fa.size <-> fa.reduceMap(_ => 1L)
}

object ReducibleLaws {
  def apply[F[_]] given Reducible[F]: ReducibleLaws[F] =
    new ReducibleLaws[F] with FoldableLaws[F] with UnorderedFoldableLaws[F]
}
