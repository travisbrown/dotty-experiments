package io.circe.cats.kernel.laws

import io.circe.cats.kernel.CommutativeSemigroup

trait CommutativeSemigroupLaws[A] given (A: CommutativeSemigroup[A]) extends SemigroupLaws[A] {
  def commutative(x: A, y: A): IsEq[A] =
    A.combine(x, y) <-> A.combine(y, x)
}

object CommutativeSemigroupLaws {
  def apply[A] given CommutativeSemigroup[A]: CommutativeSemigroupLaws[A] =
    new CommutativeSemigroupLaws[A] with SemigroupLaws[A]
}
