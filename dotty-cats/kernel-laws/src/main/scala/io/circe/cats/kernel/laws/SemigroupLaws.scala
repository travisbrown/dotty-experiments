package io.circe.cats.kernel.laws

import io.circe.cats.kernel.Semigroup

trait SemigroupLaws[A] given (A: Semigroup[A]) {
  def semigroupAssociative(x: A, y: A, z: A): IsEq[A] =
    A.combine(A.combine(x, y), z) <-> A.combine(x, A.combine(y, z))

  def repeat1(a: A): IsEq[A] =
    A.combineN(a, 1) <-> a

  def repeat2(a: A): IsEq[A] =
    A.combineN(a, 2) <-> A.combine(a, a)

  def combineAllOption(xs: Vector[A]): IsEq[Option[A]] =
    A.combineAllOption(xs) <-> xs.reduceOption(A.combine)
}

object SemigroupLaws {
  def apply[A] given Semigroup[A]: SemigroupLaws[A] =
    new SemigroupLaws[A] {}
}
