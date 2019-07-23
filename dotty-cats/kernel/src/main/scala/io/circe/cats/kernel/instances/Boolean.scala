package io.circe.cats.kernel.instances

import io.circe.cats.kernel.{Hash, LowerBounded, Order, PartialOrder, UpperBounded}

private[kernel] object BooleanInstance extends Order[Boolean] with Hash[Boolean] with LowerBounded[Boolean] with UpperBounded[Boolean] {
  override def minBound: Boolean = false
  override def maxBound: Boolean = true

  def hash(x: Boolean): Int = x.hashCode()
  def compare(x: Boolean, y: Boolean): Int =
    java.lang.Boolean.compare(x, y)

  override def eqv(x: Boolean, y: Boolean): Boolean = x == y
  override def neqv(x: Boolean, y: Boolean): Boolean = x != y
  override def gt(x: Boolean, y: Boolean): Boolean = x && !y
  override def lt(x: Boolean, y: Boolean): Boolean = !x && y
  override def gteqv(x: Boolean, y: Boolean): Boolean = x == y || x
  override def lteqv(x: Boolean, y: Boolean): Boolean = x == y || y

  override def min(x: Boolean, y: Boolean): Boolean = x && y
  override def max(x: Boolean, y: Boolean): Boolean = x || y

  override def partialOrder: PartialOrder[Boolean] = this
}
