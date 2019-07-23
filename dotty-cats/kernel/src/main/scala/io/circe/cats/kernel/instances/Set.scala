package io.circe.cats.kernel.instances

import io.circe.cats.kernel.{Hash, PartialOrder}

private[kernel] class SetInstance[A] extends PartialOrder[Set[A]] with Hash[Set[A]] {
  def partialCompare(x: Set[A], y: Set[A]): Double =
    if (x eq y) 0.0
    else if (x.size < y.size) if (x.subsetOf(y)) -1.0 else Double.NaN
    else if (y.size < x.size) if (y.subsetOf(x)) 1.0 else Double.NaN
    else if (x == y) 0.0
    else Double.NaN

  // Does not require an Eq on elements: Scala sets must use the universal `equals`.
  override def eqv(x: Set[A], y: Set[A]): Boolean = x == y

  // Does not require a Hash on elements: Scala sets must use the universal `hashCode`.
  def hash(x: Set[A]): Int = x.hashCode()
}
