package io.circe.cats.kernel.instances

import io.circe.cats.kernel.{Eq, Hash, Monoid, Order, PartialOrder}
import scala.annotation.tailrec

private[kernel] class ListOrder[A] given (A: Order[A]) extends Order[List[A]] {
  def compare(xs: List[A], ys: List[A]): Int = {
    @tailrec def loop(xs: List[A], ys: List[A]): Int =
      xs match {
        case Nil =>
          if (ys.isEmpty) 0 else -1
        case x :: xs =>
          ys match {
            case Nil => 1
            case y :: ys =>
              val n = A.compare(x, y)
              if (n != 0) n else loop(xs, ys)
          }
      }
    if (xs eq ys) 0 else loop(xs, ys)
  }
}

private[kernel] class ListPartialOrder[A] given (A: PartialOrder[A]) extends PartialOrder[List[A]] {
  def partialCompare(xs: List[A], ys: List[A]): Double = {
    @tailrec def loop(xs: List[A], ys: List[A]): Double =
      xs match {
        case Nil =>
          if (ys.isEmpty) 0.0 else -1.0
        case x :: xs =>
          ys match {
            case Nil => 1.0
            case y :: ys =>
              val n = A.partialCompare(x, y)
              if (n != 0.0) n else loop(xs, ys)
          }
      }
    if (xs eq ys) 0.0 else loop(xs, ys)
  }
}

private[kernel] class ListHash[A] given Hash[A] extends ListEq[A] with Hash[List[A]] {
  def hash(x: List[A]): Int = StaticMethods.listHash(x)
}

private[kernel] class ListEq[A] given (A: Eq[A]) extends Eq[List[A]] {
  def eqv(xs: List[A], ys: List[A]): Boolean = {
    def loop(xs: List[A], ys: List[A]): Boolean =
      xs match {
        case Nil =>
          ys.isEmpty
        case x :: xs =>
          ys match {
            case y :: ys =>
              if (A.eqv(x, y)) loop(xs, ys) else false
            case Nil =>
              false
          }
      }
    (xs eq ys) || loop(xs, ys)
  }
}

private[kernel] class ListMonoid[A] extends Monoid[List[A]] {
  def empty: List[A] = Nil
  def combine(x: List[A], y: List[A]): List[A] = x ::: y

  override def combineN(x: List[A], n: Int): List[A] =
    StaticMethods.combineNIterable(List.newBuilder[A], x, n)

  override def combineAll(xs: TraversableOnce[List[A]]): List[A] =
    StaticMethods.combineAllIterable(List.newBuilder[A], xs)
}
