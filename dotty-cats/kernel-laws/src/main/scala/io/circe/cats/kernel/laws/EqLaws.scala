package io.circe.cats.kernel.laws

import io.circe.cats.kernel.Eq

trait EqLaws[A] given (A: Eq[A]) {
  def reflexivityEq(x: A): IsEq[A] =
    x <-> x

  def symmetryEq(x: A, y: A): IsEq[Boolean] =
    A.eqv(x, y) <-> A.eqv(y, x)

  def antiSymmetryEq(x: A, y: A, f: A => A): IsEq[Boolean] =
    (!A.eqv(x, y) || A.eqv(f(x), f(y))) <-> true

  def transitivityEq(x: A, y: A, z: A): IsEq[Boolean] =
    (!(A.eqv(x, y) && A.eqv(y, z)) || A.eqv(x, z)) <-> true
}

object EqLaws {
  def apply[A] given Eq[A]: EqLaws[A] =
    new EqLaws[A] {}
}
