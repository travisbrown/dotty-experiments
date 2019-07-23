package io.circe.cats.kernel.instances

import io.circe.cats.kernel.{CommutativeGroup, Hash, LowerBounded, Order, PartialOrder, UpperBounded}

private[kernel] object LongInstance extends Order[Long] with Hash[Long] with LowerBounded[Long] with UpperBounded[Long] with CommutativeGroup[Long] {
  def combine(x: Long, y: Long): Long = x + y
  def empty: Long = 0L
  def inverse(x: Long): Long = -x
  override def remove(x: Long, y: Long): Long = x - y

  override def minBound: Long = Long.MinValue
  override def maxBound: Long = Long.MaxValue

  def hash(x: Long): Int = x.hashCode()
  def compare(x: Long, y: Long): Int =
    java.lang.Long.compare(x, y)

  override def eqv(x: Long, y: Long): Boolean = x == y
  override def neqv(x: Long, y: Long): Boolean = x != y
  override def gt(x: Long, y: Long): Boolean = x > y
  override def gteqv(x: Long, y: Long): Boolean = x >= y
  override def lt(x: Long, y: Long): Boolean = x < y
  override def lteqv(x: Long, y: Long): Boolean = x <= y

  override def min(x: Long, y: Long): Long =
    java.lang.Math.min(x, y)
  override def max(x: Long, y: Long): Long =
    java.lang.Math.max(x, y)

  def partialOrder: PartialOrder[Long] = this
}
