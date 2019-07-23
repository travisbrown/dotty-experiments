package io.circe.cats.kernel.instances

import io.circe.cats.kernel.{Eq, Hash, Monoid, Order, PartialOrder, Semigroup}


private[kernel] class EitherPartialOrder[A, B] given (A: PartialOrder[A], B: PartialOrder[B]) extends PartialOrder[Either[A, B]] {
  def partialCompare(x: Either[A, B], y: Either[A, B]): Double =
    x match {
      case Left(xx) =>
        y match {
          case Left(yy) => A.partialCompare(xx, yy)
          case Right(_) => -1.0
        }
      case Right(xx) =>
        y match {
          case Left(_)   => 1.0
          case Right(yy) => B.partialCompare(xx, yy)
        }
    }
}

private[kernel] class EitherOrder[A, B] given (A: Order[A], B: Order[B]) extends Order[Either[A, B]] {
  def compare(x: Either[A, B], y: Either[A, B]): Int =
    x match {
      case Left(xx) =>
        y match {
          case Left(yy) => A.compare(xx, yy)
          case Right(_) => -1
        }
      case Right(xx) =>
        y match {
          case Left(_)   => 1
          case Right(yy) => B.compare(xx, yy)
        }
    }
}

private[kernel] class EitherHash[A, B] given (A: Hash[A], B: Hash[B]) extends EitherEq[A, B] with Hash[Either[A, B]] {
  def hash(x: Either[A, B]): Int =
    x match {
      case Left(xx)  => StaticMethods.product1HashWithPrefix(A.hash(xx), "Left")
      case Right(xx) => StaticMethods.product1HashWithPrefix(B.hash(xx), "Right")
    }
}

private[kernel] class EitherEq[A, B] given (A: Eq[A], B: Eq[B]) extends Eq[Either[A, B]] {
  def eqv(x: Either[A, B], y: Either[A, B]): Boolean =
    x match {
      case Left(xx) =>
        y match {
          case Left(yy) => A.eqv(xx, yy)
          case Right(_) => false
        }
      case Right(xx) =>
        y match {
          case Left(_)   => false
          case Right(yy) => B.eqv(xx, yy)
        }
    }
}

private[kernel] class EitherSemigroup[A, B] given (A: Semigroup[A], B: Semigroup[B]) extends Semigroup[Either[A, B]] {
  def combine(x: Either[A, B], y: Either[A, B]): Either[A, B] =
    x match {
      case left @ Left(_) => left
      case Right(xx) =>
        y match {
          case left @ Left(_) => left
          case Right(yy)      => Right(B.combine(xx, yy))
        }
    }
}

private[kernel] class EitherMonoid[A, B] given (A: Monoid[A], B: Monoid[B]) extends Monoid[Either[A, B]] {
  def empty: Either[A, B] =
    Right(B.empty)
  def combine(x: Either[A, B], y: Either[A, B]): Either[A, B] =
    x match {
      case left @ Left(_) => left
      case Right(xx) =>
        y match {
          case left @ Left(_) => left
          case Right(yy)      => Right(B.combine(xx, yy))
        }
    }
}
