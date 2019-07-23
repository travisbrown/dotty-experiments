package io.circe.cats.kernel.instances

import io.circe.cats.kernel.{Eq, Hash, Monoid, Order, PartialOrder}

private[kernel] class VectorOrder[A] given Order[A] extends Order[Vector[A]] {
  def compare(xs: Vector[A], ys: Vector[A]): Int =
    if (xs eq ys) 0
    else StaticMethods.iteratorCompare(xs.iterator, ys.iterator)
}

private[kernel] class VectorPartialOrder[A] given PartialOrder[A] extends PartialOrder[Vector[A]] {
  def partialCompare(xs: Vector[A], ys: Vector[A]): Double =
    if (xs eq ys) 0.0
    else StaticMethods.iteratorPartialCompare(xs.iterator, ys.iterator)
}

private[kernel] class VectorHash[A] given Hash[A] extends VectorEq[A] with Hash[Vector[A]] {
  def hash(xs: Vector[A]): Int = StaticMethods.orderedHash(xs)
}

private[kernel] class VectorEq[A] given Eq[A] extends Eq[Vector[A]] {
  def eqv(xs: Vector[A], ys: Vector[A]): Boolean =
    if (xs eq ys) true
    else StaticMethods.iteratorEq(xs.iterator, ys.iterator)
}

private[kernel] class VectorMonoid[A] extends Monoid[Vector[A]] {
  def empty: Vector[A] = Vector.empty
  def combine(x: Vector[A], y: Vector[A]): Vector[A] = x ++ y

  override def combineN(x: Vector[A], n: Int): Vector[A] =
    StaticMethods.combineNIterable(Vector.newBuilder[A], x, n)

  override def combineAll(xs: TraversableOnce[Vector[A]]): Vector[A] =
    StaticMethods.combineAllIterable(Vector.newBuilder[A], xs)
}
