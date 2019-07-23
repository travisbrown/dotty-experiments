package io.circe.cats.kernel.laws

import io.circe.cats.kernel.{LowerBounded, PartialOrder, UpperBounded}

trait LowerBoundedLaws[A] given (A: LowerBounded[A]) extends PartialOrderLaws[A] {
  def boundLteqv(x: A): IsEq[Boolean] =
    A.partialOrder.lteqv(A.minBound, x) <-> true
}

object LowerBoundedLaws {
  def apply[A] given (A: LowerBounded[A]): LowerBoundedLaws[A] =
    new LowerBoundedLaws[A] with (PartialOrderLaws[A] given A.partialOrder) with (EqLaws[A] given A.partialOrder)
}

trait UpperBoundedLaws[A] given (A: UpperBounded[A]) extends PartialOrderLaws[A] {
  def boundGteqv(x: A): IsEq[Boolean] =
    A.partialOrder.gteqv(A.maxBound, x) <-> true
}

object UpperBoundedLaws {
  def apply[A] given (A: UpperBounded[A]): UpperBoundedLaws[A] =
    new UpperBoundedLaws[A] with (PartialOrderLaws[A] given A.partialOrder) with (EqLaws[A] given A.partialOrder)
}
