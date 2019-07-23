package io.circe.cats.kernel.laws

import io.circe.cats.kernel.Group

trait GroupLaws[A] given (A: Group[A]) extends MonoidLaws[A] {
  def leftInverse(x: A): IsEq[A] =
    A.empty <-> A.combine(A.inverse(x), x)

  def rightInverse(x: A): IsEq[A] =
    A.empty <-> A.combine(x, A.inverse(x))

  def consistentInverse(x: A, y: A): IsEq[A] =
    A.remove(x, y) <-> A.combine(x, A.inverse(y))
}

object GroupLaws {
  def apply[A] given Group[A]: GroupLaws[A] =
    new GroupLaws[A] with MonoidLaws[A] with SemigroupLaws[A]
}
