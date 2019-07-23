package io.circe.cats.kernel.laws

import io.circe.cats.kernel.{ Eq, Monoid }

trait MonoidLaws[A] given (A: Monoid[A]) extends SemigroupLaws[A] {
  def leftIdentity(x: A): IsEq[A] =
    A.combine(A.empty, x) <-> x

  def rightIdentity(x: A): IsEq[A] =
    A.combine(x, A.empty) <-> x

  def repeat0(x: A): IsEq[A] =
    A.combineN(x, 0) <-> A.empty

  def collect0(x: A): IsEq[A] =
    A.combineAll(Nil) <-> A.empty

  def combineAll(xs: Vector[A]): IsEq[A] =
    A.combineAll(xs) <-> (A.empty +: xs).reduce(A.combine)

  def isId(x: A) given Eq[A]: IsEq[Boolean] =
    Eq.eqv(x, A.empty) <-> A.isEmpty(x)

}

object MonoidLaws {
  def apply[A] given Monoid[A]: MonoidLaws[A] =
    new MonoidLaws[A] with SemigroupLaws[A]
}
