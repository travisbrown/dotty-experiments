package io.circe.cats.kernel.instances

import io.circe.cats.kernel.{Eq, Hash, Monoid, Order, PartialOrder}
import scala.collection.immutable.Queue

private[kernel] class QueueOrder[A] given Order[A] extends Order[Queue[A]] {
  def compare(xs: Queue[A], ys: Queue[A]): Int =
    if (xs eq ys) 0
    else StaticMethods.iteratorCompare(xs.iterator, ys.iterator)
}

private[kernel] class QueueHash[A] given Hash[A] extends QueueEq[A] with Hash[Queue[A]] {
  def hash(x: Queue[A]): Int = StaticMethods.orderedHash(x)
}

private[kernel] class QueuePartialOrder[A] given PartialOrder[A] extends PartialOrder[Queue[A]] {
  def partialCompare(xs: Queue[A], ys: Queue[A]): Double =
    if (xs eq ys) 0.0
    else StaticMethods.iteratorPartialCompare(xs.iterator, ys.iterator)
}

private[kernel] class QueueEq[A] given Eq[A] extends Eq[Queue[A]] {
  def eqv(xs: Queue[A], ys: Queue[A]): Boolean =
    if (xs eq ys) true
    else StaticMethods.iteratorEq(xs.iterator, ys.iterator)
}

private[kernel] class QueueMonoid[A] extends Monoid[Queue[A]] {
  def empty: Queue[A] = Queue.empty[A]
  def combine(x: Queue[A], y: Queue[A]): Queue[A] = x ++ y

  override def combineN(x: Queue[A], n: Int): Queue[A] =
    StaticMethods.combineNIterable(Queue.newBuilder[A], x, n)

  override def combineAll(xs: TraversableOnce[Queue[A]]): Queue[A] =
    StaticMethods.combineAllIterable(Queue.newBuilder[A], xs)
}
