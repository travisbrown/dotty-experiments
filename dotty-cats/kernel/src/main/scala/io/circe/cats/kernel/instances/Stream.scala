package io.circe.cats.kernel.instances

import io.circe.cats.kernel.{Eq, Hash, Monoid, Order, PartialOrder}

private[kernel] class StreamOrder[A] given Order[A] extends Order[Stream[A]] {
  def compare(xs: Stream[A], ys: Stream[A]): Int =
    if (xs eq ys) 0
    else StaticMethods.iteratorCompare(xs.iterator, ys.iterator)
}

private[kernel] class StreamPartialOrder[A] given PartialOrder[A] extends PartialOrder[Stream[A]] {
  def partialCompare(xs: Stream[A], ys: Stream[A]): Double =
    if (xs eq ys) 0.0
    else StaticMethods.iteratorPartialCompare(xs.iterator, ys.iterator)
}

private[kernel] class StreamHash[A] given Hash[A] extends StreamEq[A] with Hash[Stream[A]] {
  def hash(xs: Stream[A]): Int = StaticMethods.orderedHash(xs)
}

private[kernel] class StreamEq[A] given Eq[A] extends Eq[Stream[A]] {
  def eqv(xs: Stream[A], ys: Stream[A]): Boolean =
    if (xs eq ys) true
    else StaticMethods.iteratorEq(xs.iterator, ys.iterator)
}

private[kernel] class StreamMonoid[A] extends Monoid[Stream[A]] {
  def empty: Stream[A] = Stream.empty
  def combine(x: Stream[A], y: Stream[A]): Stream[A] = x ++ y
  override def combineN(x: Stream[A], n: Int): Stream[A] =
    StaticMethods.combineNIterable(Stream.newBuilder[A], x, n)

  override def combineAll(xs: TraversableOnce[Stream[A]]): Stream[A] =
    StaticMethods.combineAllIterable(Stream.newBuilder[A], xs)
}
