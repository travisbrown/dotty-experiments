package io.circe.cats.kernel.instances

import io.circe.cats.kernel.{CommutativeGroup, Hash, LowerBounded, Order, PartialOrder, UpperBounded}

private[kernel] object ShortInstance extends Order[Short] with Hash[Short] with LowerBounded[Short] with UpperBounded[Short] with CommutativeGroup[Short] {
  def combine(x: Short, y: Short): Short = (x + y).toShort
  def empty: Short = 0
  def inverse(x: Short): Short = (-x).toShort
  override def remove(x: Short, y: Short): Short = (x - y).toShort
  override def minBound: Short = Short.MinValue
  override def maxBound: Short = Short.MaxValue

  def hash(x: Short): Int = x.hashCode()
  def compare(x: Short, y: Short): Int =
    java.lang.Short.compare(x, y)

  override def eqv(x: Short, y: Short): Boolean = x == y
  override def neqv(x: Short, y: Short): Boolean = x != y
  override def gt(x: Short, y: Short): Boolean = x > y
  override def gteqv(x: Short, y: Short): Boolean = x >= y
  override def lt(x: Short, y: Short): Boolean = x < y
  override def lteqv(x: Short, y: Short): Boolean = x <= y

  override def min(x: Short, y: Short): Short =
    java.lang.Math.min(x.toInt, y.toInt).toShort
  override def max(x: Short, y: Short): Short =
    java.lang.Math.max(x.toInt, y.toInt).toShort

  override def partialOrder: PartialOrder[Short] = this
}
