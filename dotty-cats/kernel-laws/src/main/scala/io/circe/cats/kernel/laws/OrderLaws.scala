package io.circe.cats.kernel.laws

import io.circe.cats.kernel.Order

trait OrderLaws[A] given (A: Order[A]) extends PartialOrderLaws[A] {
  def totality(x: A, y: A): IsEq[Boolean] =
    (A.lteqv(x, y) || A.lteqv(y, x)) <-> true

  def compare(x: A, y: A): IsEq[Boolean] = {
    val c = A.compare(x, y)
    (((c < 0) == A.lt(x, y)) && ((c == 0) == A.eqv(x, y)) && ((c > 0) == A.gt(x, y))) <-> true
  }

  def min(x: A, y: A): IsEq[Boolean] = {
    val c = A.compare(x, y)
    val m = A.min(x, y)
    if (c < 0) A.eqv(m, x) <-> true
    else if (c == 0) (A.eqv(m, x) && (A.eqv(m, y))) <-> true
    else A.eqv(m, y) <-> true
  }

  def max(x: A, y: A): IsEq[Boolean] = {
    val c = A.compare(x, y)
    val m = A.max(x, y)
    if (c < 0) A.eqv(m, y) <-> true
    else if (c == 0) (A.eqv(m, x) && (A.eqv(m, y))) <-> true
    else A.eqv(m, x) <-> true
  }

}

object OrderLaws {
  def apply[A] given Order[A]: OrderLaws[A] =
    new OrderLaws[A] with PartialOrderLaws[A] with EqLaws[A]
}
