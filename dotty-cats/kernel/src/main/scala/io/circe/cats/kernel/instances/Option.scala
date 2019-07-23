package io.circe.cats.kernel.instances

import io.circe.cats.kernel.{Eq, Hash, Monoid, Order, PartialOrder, Semigroup}

private[kernel] class OptionOrder[A] given (A: Order[A]) extends Order[Option[A]] {
  def compare(x: Option[A], y: Option[A]): Int =
    x match {
      case None =>
        if (y.isEmpty) 0 else -1
      case Some(a) =>
        y match {
          case None    => 1
          case Some(b) => A.compare(a, b)
        }
    }
}

private[kernel] class OptionPartialOrder[A] given (A: PartialOrder[A]) extends PartialOrder[Option[A]] {
  def partialCompare(x: Option[A], y: Option[A]): Double =
    x match {
      case None =>
        if (y.isEmpty) 0.0 else -1.0
      case Some(a) =>
        y match {
          case None    => 1.0
          case Some(b) => A.partialCompare(a, b)
        }
    }
}

private[kernel] class OptionHash[A] given (A: Hash[A]) extends OptionEq[A] with Hash[Option[A]] {
  def hash(x: Option[A]): Int = x match {
    case None     => None.hashCode()
    case Some(xx) => StaticMethods.product1HashWithPrefix(A.hash(xx), x.productPrefix)
  }
}

private[kernel] class OptionEq[A] given (A: Eq[A]) extends Eq[Option[A]] {
  def eqv(x: Option[A], y: Option[A]): Boolean =
    x match {
      case None => y.isEmpty
      case Some(a) =>
        y match {
          case None    => false
          case Some(b) => A.eqv(a, b)
        }
    }
}

private[kernel] class OptionMonoid[A] given (A: Semigroup[A]) extends Monoid[Option[A]] {
  def empty: Option[A] = None
  def combine(x: Option[A], y: Option[A]): Option[A] =
    x match {
      case None => y
      case Some(a) =>
        y match {
          case None    => x
          case Some(b) => Some(A.combine(a, b))
        }
    }
}
