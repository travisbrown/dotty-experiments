package io.circe.cats.kernel.instances

import io.circe.cats.kernel.{CommutativeGroup, Hash, LowerBounded, Order, PartialOrder, UpperBounded}

private[kernel] object IntInstance extends Order[Int] with Hash[Int] with LowerBounded[Int] with UpperBounded[Int] with CommutativeGroup[Int] {
  def combine(x: Int, y: Int): Int = x + y
  def empty: Int = 0
  def inverse(x: Int): Int = -x
  override def remove(x: Int, y: Int): Int = x - y

  override def minBound: Int = Int.MinValue
  override def maxBound: Int = Int.MaxValue

  def hash(x: Int): Int = x.hashCode()
  def compare(x: Int, y: Int): Int =
    java.lang.Integer.compare(x, y)

  override def eqv(x: Int, y: Int): Boolean = x == y
  override def neqv(x: Int, y: Int): Boolean = x != y
  override def gt(x: Int, y: Int): Boolean = x > y
  override def gteqv(x: Int, y: Int): Boolean = x >= y
  override def lt(x: Int, y: Int): Boolean = x < y
  override def lteqv(x: Int, y: Int): Boolean = x <= y

  override def min(x: Int, y: Int): Int =
    java.lang.Math.min(x, y)
  override def max(x: Int, y: Int): Int =
    java.lang.Math.max(x, y)

  def partialOrder: PartialOrder[Int] = this
}
