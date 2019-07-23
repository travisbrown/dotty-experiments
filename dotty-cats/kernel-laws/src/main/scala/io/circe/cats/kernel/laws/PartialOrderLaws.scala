package io.circe.cats.kernel.laws

import io.circe.cats.kernel.{Eq, PartialOrder}

trait PartialOrderLaws[A] given (A: PartialOrder[A]) extends EqLaws[A] {

  def reflexivityLt(x: A): IsEq[Boolean] =
    A.lteqv(x, x) <-> true

  def reflexivityGt(x: A): IsEq[Boolean] =
    A.gteqv(x, x) <-> true

  def antisymmetry(x: A, y: A): IsEq[Boolean] =
    (!(A.lteqv(x, y) && A.lteqv(y, x)) || A.eqv(x, y)) <-> true

  def transitivity(x: A, y: A, z: A): IsEq[Boolean] =
    (!(A.lteqv(x, y) && A.lteqv(y, z)) || A.lteqv(x, z)) <-> true

  def gteqv(x: A, y: A): IsEq[Boolean] =
    A.lteqv(x, y) <-> A.gteqv(y, x)

  def lt(x: A, y: A): IsEq[Boolean] =
    A.lt(x, y) <-> (A.lteqv(x, y) && A.neqv(x, y))

  def gt(x: A, y: A): IsEq[Boolean] =
    A.lt(x, y) <-> A.gt(y, x)

  def partialCompare(x: A, y: A): IsEq[Boolean] = {
    val c = A.partialCompare(x, y)
    (((c < 0) == A.lt(x, y)) && ((c == 0) == A.eqv(x, y)) && ((c > 0) == A.gt(x, y))) <-> true
  }

  def pmin(x: A, y: A): IsEq[Boolean] = {
    val c = A.partialCompare(x, y)
    val m = A.pmin(x, y)
    if (c < 0) Eq[Option[A]].eqv(m, Option(x)) <-> true
    else if (c == 0) (Eq[Option[A]].eqv(m, Option(x)) && Eq[Option[A]].eqv(m, Option(y))) <-> true
    else if (c > 0) Eq[Option[A]].eqv(m, Option(y)) <-> true
    else Eq[Option[A]].eqv(m, None) <-> true
  }

  def pmax(x: A, y: A): IsEq[Boolean] = {
    val c = A.partialCompare(x, y)
    val m = A.pmax(x, y)
    if (c < 0) Eq[Option[A]].eqv(m, Option(y)) <-> true
    else if (c == 0) (Eq[Option[A]].eqv(m, Option(x)) && Eq[Option[A]].eqv(m, Option(y))) <-> true
    else if (c > 0) Eq[Option[A]].eqv(m, Option(x)) <-> true
    else Eq[Option[A]].eqv(m, None) <-> true
  }

}

object PartialOrderLaws {
  def apply[A] given PartialOrder[A]: PartialOrderLaws[A] =
    new PartialOrderLaws[A] with EqLaws[A]
}
